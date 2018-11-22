##
# this project is still a work in progress.
# the language could change such that things break.
# documentation could be improved.
# code is prototype-ish.
##

16 "registers", r0,r1,r2,...,r15
active "register" is pointer to one of r0,r1,...
operator "registers" op0, and op1 is pointer to one of r0,r1,...
operand size influenced by op0sz, and op1sz

defaults to 32MB of memory on startup.
op0 -> r14, op0sz -> 8
op1 -> r15, op1sz -> 8
ar -> r0

stack segment points to the end of memory and grows down by sizeof(long).
default stack segment size is 16KB

function map starts at end of stack segment and grows down by 2*sizeof(long).
default function map size is 16KB

[:space:]   - ignored
[:newline:] - ignored
[0-9A-Fa-f] - input as hex value

    .       - print ar to stdout
    ,       - read stdin to ar


    #       - ignore until newline.
    #%a     - Debug. (dump all machinestate) (equiv to #%g#%p#%r#%v)
    #%g     - Debug. (Dump generic information.)
    #%p     - Debug. (Dump pointer information.)
    #%r     - Debug. (Dump registers, long view)
    #%v     - Debug. (Dump variables)
    #%q     - Debug. (Dump registers and pointers short view)
 #%[0-9A-F] - Debug. (Dump register r0,r1,r2,...,rN}
    #%s     - Debug. (Print string at mem[op0])
    #%m     - Debug. (Print bytes mem[op0] through mem[op1])

    :N      - copy value in ar to register rN
    ::      - copy value in ar to operator register 0
    :;      - copy value in ar to operator register 1
    :$      - copy value in ar to mem[mp], incrementing mp by operand size of op0.
    :S      - copy value in ar to mem[mp], not touching mp.
    :m      - copy value in ar to mp.
  :[var]    - copy value in ar to var.
    :o      - set op0 to value in ar.
    :O      - set op1 to value in ar.
   :{N}     - copy value in ar to mem[value in rN] of op0sz.
   :{{N}}   - copy value in ar to mem[value in rN] of op0sz, incrementing rN by op0sz.


    ;N      - copy value of register rN to ar
    ;:      - copy value of operator register 0 to ar
    ;;      - copy value of operator register 1 to ar
    ;$      - copy value of mem[mp] to ar, incrementing mp by operand size of op0.
    ;S      - copy value of mem[mp] to ar, not touching mp.
    ;m      - copy value of mp to ar.
  ;[var]    - copy value of var to ar.
    ;_      - copy active register index to active register.
    ;o      - copy op0 index to active register.
    ;O      - copy op1 index to active register.
   ;{N}     - copy value of mem[value of rN] of op0sz to ar.
   ;{{N}}   - copy value of mem[value of rN] of op0sz to ar, incrementing rN by op0sz.

    _N      - set active register to register rN
    _:      - set active register to operator register 0
    _;      - set active register to operator register 1 
    __      - set active register to value in ar.

  S[var]    - copy value of mp to var.
  $[var]    - set mp to value in var.
  T[var]    - destroy var.

    $<      - decrement mp by value in ar.
    $>      - increment mp by value in ar.
    $$      - shortcut to function 3. (string)
    $+      - increment mp by value in op0*op1;
    $-      - decrement mp by value in op0*op1;

    qAB     - assign value in register A to register B.
            - A, and B can be one of:
            -   [0-f] for r0,r1,r2,...,r15
            -   m for mp, : for op0, ; for op1 or _ for ar.

    ~AB     - use registers A, and B for operator register 0 and 1.
    ~~N     - N=1,2,4,8; set operand size for opX, X found in ar. (X=0,1)
    ~~_     - set registers op0, and op1 to register values found in op0, and op1 respectively.
    ~~~     - Binary NOT of op0 OR op1. (~(op0|op1))

    +       - Add op0 and op1 registers, storing output in ar.
    -       - Subtract operator registers, storing output in ar.
    *       - Multiply operator registers, storing output in ar.
    /       - Divide operator registers, storing output in ar.
    %       - Modulo operator registers, storing output in ar.
    &       - Binary AND of operator registers, storing output in ar.
    |       - Binary OR of operator registers, storing output in ar.
    >       - Binary Right shift of operator registers, storing output in ar.
    <       - Binary Left shift of operator registers, storing output in ar.
    =       - Compare value of op0, and op1 via xor.
            - ar will be 0 if op0 and op1 match.
            - ar will be non-zero if op0 and op1 dont match.
	    --- Operations are performed as *ar = *op0 <op> *op1;

  ?(!code); - Execute code if ar is 0.
  ?(?code); - Execute code if ar is non-zero.
  ?(?code)? - if(ar) { foo: code if(ar) goto foo; }
  ?(?code)! - if(ar) { foo: code if(!ar) goto foo; }
  ?(!code)? - if(!ar) { foo: code if(ar) goto foo; }
  ?(!code)! - if(!ar) { foo: code if(!ar) goto foo; }
  ?(<code)< - conditional, if(op0 < op1)
  ?(>code)> - conditional, if(op0 > op1)
  ?(=code)= - conditional, if(op0 == op1)
  ?(/code)/ - conditional, if(op0 != op1)
  ?(]code)] - conditional, if(op0 >= op1)
  ?([code)[ - conditional, if(op0 <= op1)

    ^       - pushes value of ar onto stack segment
    v       - pops value on stack segment into ar

  "string"  - loads 'string' into mem[mp], incrementing mp.
            - escape sequences supported are: \", \\, \n, and \xHEX
            - op0 is assigned the start address of "string"
            - op1 is assigned the end address of "string\0"
            - ar is assigned the length of "string" (6)

 [ n n n ]  - Load array of integers n into memory of byte size opsz[0] each.
            - Uses opsz[0] as increment.
            - op0 is assigned the start address of the array
            - op1 is assigned the end address of the array
            - ar is assigned the length of the array.


    @       - call function in ar.

  {code}    - declares {code} as a function, the function number is in ar.

  '<char>'  - writes byte value of <char> to ar.
            - Escape characters supported are: '\r', '\n', '\\', '\'', and '\xHEX'

  `string`  - write value 'string' into ar.
            - Note 1: this operation ignores operation sizes.
	    - Note 2: the string is limited to sizeof(long) bytes. (8 on x86_64 platforms.)

    !!bb    - use base bb as input base. Note: this only changes the multiplier to bb.
              Hex input will still be available. This does not effect
              operators. (eg, :11 wont work, you will still need to use :b)
	          a-f retain their values of 10-15. Likewise, increasing the base
	          does not expose more input characters. You will still be stuck
	          with 0-F (0-15) as the units value for a given base.
	          Setting !!00 will create the illusion of only having values 0-f.

     !(<N){code}   - For loop. This is the equivilent of doing something like:
                     for(*ar = *op0; *ar < N; *ar += *op1) { /* code */ }
     !(>N){code}   - For loop. Like above, but use > comparison, (step is -= *op1)
     !(=N){code}   - For loop. Like above, but use == comparison, (step is += *op1)
    !(<=N){code}   - For loop. Like above, but use <= comparison, (step is += *op1)
    !(>=N){code}   - For loop. Like above, but use >= comparison. (step is -= *op1)
    !(!=N){code}   - For loop. Like above, but use != comparison, (step is += *op1)
    !(==N){code}   - For loop. Like above, but use == comparison. (step is -= *op1)
  !(<op>;R){code}  - For loop. Use current value of register R
 !(<op>;(R)){code} - For loop. Use value of register R. Keeping up with register R even if it changes.

####################
# Core Function list

    0       - exit with value in op0

    1       - write function
            - r1 = fd
            - r2 = memory location
            - r3 = length
            - output: ar = write(2)
            
    2       - read function
            - r1 = fd
            - r2 = memory location
            - r3 = length
            - output: ar = read(2)

    3       - string function
            - op0 = memory location of string
            - output: ar = strlen(string)
            - output: op0 = memory address of '\0' byte in string.
            - output: op1 = memory address just past '\0' byte in string.
