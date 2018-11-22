#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <sys/select.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <ctype.h>
#include <limits.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <err.h>
#include <locale.h>

#define safe_print(ch)		(isprint((ch)) ? (int) (ch) : '.')

struct loop_object_s {
	long lo_start_addr;
	long lo_end_addr;
	long lo_change;
	long lo_current;
	long lo_start;

	union {
		struct {
			long r_rip;
			long r_flags;
		} f_rip;

		struct {
			long l_end;
			long l_break;
		} f_loop;
	} lo_footer;
} __attribute__ ((packed));

#define LO_LENGTH	(sizeof(struct loop_object_s)/sizeof(long))
#define LO_OBJECT(obj)	((struct loop_object_s *) (obj));
/*
 * f = (op << 4) | (register+1);
 * f = (f << 4) | type;
 *
 * 11111111    11111111    1111
 * operator    register    type
 */
#define LOFL_GETOP(fl)	(((fl) >> 12))
#define LOFL_GETREG(fl)	(((fl) >> 4) & 0xff)
#define LOFL_GETTYP(fl) (((fl) >> 0) & 0x0f)

struct rip_object_s {
	long ro_rip;
	long ro_flags;
} __attribute__ ((packed));
/*
 * f = (f << 0) | op0sz, f = (f << 4) | op0
 * f = (f << 8) | op1sz, f = (f << 12) | op1
 * f = (f << 16) | ar, f = (f << 20) | type
 *
 *  1111   1111   1111   1111   1111   1111
 * op0sz    op0  op1sz    op1     ar   type
 */
#define ROFL_GETOPSZ(ro,op)	(((ro)->ro_flags >> (12 + ((((op) + 1) % 2) * 8))) & 0xf)
#define ROFL_GETOP(ro,op)	(((ro)->ro_flags >> ( 8 + ((((op) + 1) % 2) * 8))) & 0xf)
#define ROFL_GETAR(ro)		(((ro)->ro_flags >> 4) & 0xf)
#define ROFL_GETTYP(ro)		((ro)->ro_flags & 0xf)

#define RO_LENGTH	(sizeof(struct rip_object_s)/sizeof(long))
#define RO_OBJECT(obj)	((struct rip_object_s *) (obj));
#define LO_SIZE		((LO_LENGTH) - (RO_LENGTH))

#define ROFL_SETTYP(fl,typ)

struct list_data_s {
	char *ld_name;
	long ld_value;
	struct list_data_s *ld_next;
};

struct list_container_s {
	struct list_data_s *lc_head;
	struct list_data_s *lc_foot;
};

struct function_map_s {
	long fm_fn;		// function number.
	long fm_addr;		// function pointer.
};

struct machinestate_s {
	long ms_storage_size;	// total memory size (in bytes)
	long ms_memory_size;	// total memory size (in bytes) (-junk)
	char *ms_memory;	// pointer to memory buffer.

	long ms_code_size;	// total code size (in bytes)
	char *ms_code;		// pointer to code buffer.

	long ms_stack_size;	// stack size
	long ms_funcmap_size;	// function map size

	long ms_ip;		// instruction pointer.
	long ms_mp;		// memory pointer;

	long *ms_rip;		// return instruction pointer.
				// points to bottom of stack and grows upward.

	struct {
		long *s_stack_start, *s_stack, *s_stack_end;
		struct function_map_s *s_funcmap_start,
				      *s_funcmap, *s_funcmap_end;
	} ms_segment;

	struct {
		long r_reg[16];	// "registers"
		long r_ar;		// active register

		long *r_[2];		// register operands.
		long r_opsz[2];	// operand size.
	} ms_reg;

	struct list_container_s ms_vars;	// variable container
};

#define MSRG_GET(st, i)		((st)->ms_reg.r_reg[(i)])
#define MSRG_GETOP(st, i)	((st)->ms_reg.r_[(i)])
#define MSRG_GETCUR(st)		((st)->ms_reg.r_ar)
#define MSRG_GETAR(st)		((long *) &(st)->ms_reg.r_reg[MSRG_GETCUR(st)])
#define MSRG_GETRIPSZ(st)	((long) ((st)->ms_rip - (st)->ms_segment.s_stack_end))
#define MSRG_GETOPSZ(st, i)	((st)->ms_reg.r_opsz[i])
#define MSRG_GETOPI(st, i)	(MSRG_GETOP(st,i) - &(MSRG_GET(st,0)))
#define MSIP_PEEK(st, i)	((i) < (st)->ms_code_size ? (st)->ms_code[i] : -1)
#define LINE_CHANGE(l, c)	do { l += 1; c = 0; } while(0)
#define MSRIP_CHECK(st, size)	((long *) ((st)->ms_rip-(size)) < ((long *) (st)->ms_segment.s_stack_end))
#define MSRIP_OKTOPUSH(st,size) (!((st)->ms_rip+(size) > (st)->ms_segment.s_stack_start))
#define MSRIP_SIZE		2
#define MSRIP_POP(st, size)	((st)->ms_rip -= (size))
#define MSRIP_PUSH(st, size)	((st)->ms_rip += (size))
#define MSRIP_OPUSH(st, obj)	((st)->ms_rip += sizeof(*(obj))/sizeof(long))

struct tracestack_object_s {
	long to_size;
	void *to_obj;
}; // __attribute__ ((packed));

enum {
	RTYP_FUNCTION = 0,
	RTYP_FORLOOP,

	TS_ADD,
	TS_SHOW,
	TS_CLEANUP,
};

static int input_base = 16;	// eww.
static int gdebug = 0;

long nextop(struct machinestate_s *state, int expect, ...);
#define vmop(state, exp, ...) nextop((state), (exp), __VA_ARGS__, NULL)

struct core_functions_s {
	long cf_fn;	// function number.
	void (*func)(struct machinestate_s *);	// function pointer.
};

#define err_need_args(token, addr) \
	do { \
		warnx("Invalid Input at 0x08%lx", (addr)); \
		warnx("'%c' needs more arguments.", (token)); \
	} while(0);

#define err_unexpected_character(op, token, addr) \
	do { \
		warnx("Unexpected Token '%c' at 0x%08lx for op '%c'.", \
			(int)((token) & 0xff), (addr), (op)); \
	} while(0);

void translate_byte(struct machinestate_s *state, long ip, long *line, long *chr)
{
	register long i;

	*line = 1;
	*chr = 0;

	for(i = 0; i <= ip; ++i, ++(*chr)) {
		if(state->ms_code[i] == '\n') {
			if(i != ip) {
				*chr = 0;
				*line += 1;
			}
		}
	}

	if(*chr > 1)
		*chr -= 1;
}

void pdebug(struct machinestate_s *state, long ip)
{
	long ln, chr;
	translate_byte(state, ip, &ln, &chr);
	printf(" -> '%c' (0x%02x) line: %ld, character: %ld (0x%08lX) (%ld)\n",
		isprint((state)->ms_code[(ip)]) ? (state)->ms_code[(ip)] : '.',
		(state)->ms_code[(ip)], ln, chr, ip, ip);
}

inline int hex2int(int x)
{
	return ((10*(x/65)+((x+1)%49))%17);
}

struct list_data_s *new_list_item(struct list_container_s *lc, char *name, long count, long value)
{
	struct list_data_s *new;

	new = calloc(1, sizeof(*new));
	if(!new) return NULL;

	new->ld_name = calloc(1, count+1);
	if(!new->ld_name) {
		free(new);
		return NULL;
	}

	memcpy(new->ld_name, name, count);
	new->ld_name[count] = 0;
	new->ld_value = value;

	if(lc->lc_head == NULL) {
		lc->lc_head = new;
		lc->lc_foot = new;
	} else {
		lc->lc_foot->ld_next = new;
		lc->lc_foot = new;
	}

	return new;
}

struct list_data_s *find_list_item2(struct list_container_s *lcp,
		char *name, long count, struct list_container_s *rp)
{
	register struct list_data_s *ldp;
	register char *buf;
	struct list_container_s lc = {NULL,NULL};

	if(!lcp) return NULL;

	buf = calloc(1,count+1);
	if(!buf) return NULL;

	memcpy(buf,name,count);
	buf[count] = 0;

	lc.lc_head = lcp->lc_head;
	for(ldp = lcp->lc_head; ldp != NULL; ldp = ldp->ld_next) {
		if(!strcmp(buf, ldp->ld_name)) {
			lc.lc_foot = ldp->ld_next;
			goto _out;
		}
		lc.lc_head = ldp;
	}

_out:
	free(buf);
	if(rp) {
		rp->lc_head = lc.lc_head;
		rp->lc_foot = lc.lc_foot;
	}
	return ldp;
}

struct list_data_s *find_list_item(struct list_container_s *lcp, char *name, long count)
{
	return find_list_item2(lcp, name, count, NULL);
}

struct list_data_s *update_list_item(struct list_container_s *lcp, char *name, long count, long value)
{
	register struct list_data_s *ldp;

	ldp = find_list_item(lcp,name,count);
	if(!ldp) return new_list_item(lcp,name,count,value);

	ldp->ld_value = value;
	return ldp;
}

void drop_list_item(struct list_container_s *lcp, char *name, long count)
{
	struct list_data_s *item;
	struct list_container_s lc;

	item = find_list_item2(lcp, name, count, &lc);
	if(!item) return;

	free(item->ld_name);
	if(item == lcp->lc_head) {
		lcp->lc_head = item->ld_next;

		if(item == lcp->lc_foot)
			lcp->lc_foot = item->ld_next;

		free(item);
	} else if(item == lcp->lc_foot) {
		lcp->lc_foot = lc.lc_head;
		lcp->lc_foot->ld_next = lc.lc_foot;

		free(item);
	} else {
		lc.lc_head->ld_next = lc.lc_foot;
		free(item);
	}
}

void destroy_list(struct list_container_s *lcp)
{
	register struct list_data_s *ldp, *tmp;

	for(ldp = lcp->lc_head; ldp != NULL;) {
		tmp = ldp;
		ldp = ldp->ld_next;

		free(tmp->ld_name);
		free(tmp);
	}

	lcp->lc_head = NULL;
	lcp->lc_foot = NULL;
}

long ophlpr(struct machinestate_s *state, long op)
{
	register long op0=0,op1=0,*op0p,*op1p,value;

	op0p = state->ms_reg.r_[0];
	op1p = state->ms_reg.r_[1];

	switch(state->ms_reg.r_opsz[0]) {
		case 1: op0 = *((char *)op0p); break;
		case 2: op0 = *((short *)op0p); break;
		case 4: op0 = *((int *)op0p); break;
		case 8: op0 = *((long *)op0p); break;
	}

	switch(state->ms_reg.r_opsz[1]) {
		case 1: op1 = *((char *)op1p); break;
		case 2: op1 = *((short *)op1p); break;
		case 4: op1 = *((int *)op1p); break;
		case 8: op1 = *((long *)op1p); break;
	}

	switch(op) {
		case '+': value = op0 + op1; break;
		case '-': value = op0 - op1; break;
		case '*': value = op0 * op1; break;
		case '/': value = op0 / op1; break;
		case '%': value = op0 % op1; break;
		case '&': value = op0 & op1; break;
		case '|': value = op0 | op1; break;
		case '>': value = op0 >> op1; break;
		case '<': value = op0 << op1; break;
		case '~': value = ~(op0 | op1); break;
		default: value = 0; break;
	}

	return value;
}

long varhlpr(struct machinestate_s *state)
{
	register char *p, *end = state->ms_code+state->ms_code_size;
	register long count = 0;

	p = state->ms_code + state->ms_ip + 1;
	while(p < end) {
		if(*p == ']') break;
		++count, ++p;
	}

	if(*p != ']') {
		warnx("Unbalanced bracket in variable scheme");
		return -1;
	}

	return count;
}

/*$*/
void zf_string(struct machinestate_s *state);
void memhlpr(struct machinestate_s *state, long op)
{
	register long *op0, *op1, *ar = MSRG_GETAR(state), tmp;

	switch(op) {
		case '<':
			if(state->ms_mp - *ar < 0) state->ms_mp = 0;
			else state->ms_mp -= *ar;
			break;

		case '>':
			if(state->ms_mp + *ar < state->ms_memory_size)
				state->ms_mp += *ar;
			break;

		case '$':
			zf_string(state);
			break;

		case '+':
		case '-':
			op0 = MSRG_GETOP(state,0);
			op1 = MSRG_GETOP(state,1);
			tmp = *op0 * *op1;

			if(op == '+') tmp = state->ms_mp + tmp;
			else tmp = state->ms_mp - tmp;

			if(tmp < 0 || tmp >= state->ms_memory_size) {
				warnx("Attempted to access out of bounds memory");
				break;
			}

			state->ms_mp = tmp;
			break;

		default:
			warnx("Unknown character '%c' for op '$'.", (int) op);
	}
}

/*:*/
void i_cpy1ar(struct machinestate_s *state, long rN)
{
	long tmp, tmp2, tmp3;

	if(isxdigit(rN)) {
		MSRG_GET(state, hex2int(rN)) = *(MSRG_GETAR(state));
	} else if(rN == ':' || rN == ';') {
		*(MSRG_GETOP(state,rN%2)) = *(MSRG_GETAR(state));
	} else if(rN == '$' || rN == 'S') {
		tmp = state->ms_reg.r_opsz[0];
		memcpy(&state->ms_memory[state->ms_mp], MSRG_GETAR(state), tmp);

		if(rN == '$') {
			tmp = state->ms_mp;
			tmp += state->ms_reg.r_opsz[0];
			tmp %= state->ms_memory_size;

			state->ms_mp = tmp;
		}
	} else if(rN == 'm') {
		tmp = *(MSRG_GETAR(state));
		if(tmp < 0 || tmp >= state->ms_memory_size) {
			warnx("Attempted to use out of bounds memory address.\n");
			return;
		}
		state->ms_mp = tmp;
	} else if(rN == 'o' || rN == 'O') {
		if(*(MSRG_GETAR(state)) < 16) MSRG_GETOP(state,((rN/12)+1)%2) = &(MSRG_GET(state,*(MSRG_GETAR(state))));
	} else if(rN == '{') {
		tmp3 = tmp2 = tmp = 0;;

		if(!vmop(state,2,&tmp,&tmp2)) {
			warnx("Out of codes?");
			return;
		}

		if(tmp == '{') {
			if(!vmop(state,2,&tmp,&tmp3)) {
				warnx("Out of codes?");
				return;
			}

			if(tmp != '}' || tmp != tmp3) {
				warnx("Unexpected input. Expected '}}', got '%c%c' instead.", (int) tmp, (int) tmp3);
				return;
			}
			tmp = tmp2;
			tmp2 = tmp3;
		}
		if(tmp2 != '}') {
			warnx("Unexpected character. Expected '}', got '%c' instead. (%ld)", (int) tmp2, tmp2);
			return;
		}

		if(!isxdigit(tmp)) {
			warnx("Unexpected character. Expected register. got '%c' instead.", (int) tmp);
			return;
		}

		tmp2 = hex2int(tmp);
		tmp = MSRG_GET(state,tmp2);
		if(tmp < 0 || tmp > state->ms_memory_size) {
			warnx("Attempted to access out of bounds memory.");
			return;
		}

		memcpy(state->ms_memory+tmp, MSRG_GETAR(state), MSRG_GETOPSZ(state,0));
		if(tmp3) MSRG_GET(state,tmp2) += MSRG_GETOPSZ(state,0);
	} else if(rN == '[') {
		tmp = varhlpr(state);
		if(tmp < 0) return;

		update_list_item(&state->ms_vars, state->ms_code+state->ms_ip+1,
				tmp, *(MSRG_GETAR(state)));

		state->ms_ip += (tmp+1);
	} else {
		err_unexpected_character(':', rN, state->ms_ip);
	}
}

/*;*/
void i_cpy2ar(struct machinestate_s *state, long rN)
{
	long tmp, tmp2, tmp3;
	register struct list_data_s *item;

	if(isxdigit(rN)) {
		*(MSRG_GETAR(state)) = MSRG_GET(state,hex2int(rN));
	} else if(rN == ':' || rN == ';') {
		*(MSRG_GETAR(state)) = *(MSRG_GETOP(state,(rN%2)));
	} else if(rN == '$' || rN == 'S') {
		*(MSRG_GETAR(state)) = state->ms_memory[state->ms_mp];

		if(rN == '$') {
			tmp = state->ms_mp;
			tmp += state->ms_reg.r_opsz[0];
			tmp %= state->ms_memory_size;

			state->ms_mp = tmp;
		}
	} else if(rN == 'm') {
		*(MSRG_GETAR(state)) = state->ms_mp;
	} else if(rN == '_') {
		*(MSRG_GETAR(state)) = state->ms_reg.r_ar;
	} else if(rN == 'o') {
		*(MSRG_GETAR(state)) = MSRG_GETOP(state,0) - &(MSRG_GET(state,0));
	} else if(rN == 'O') {
		*(MSRG_GETAR(state)) = MSRG_GETOP(state,1) - &(MSRG_GET(state,0));
	} else if(rN == '{') {
		tmp3 = tmp2 = tmp = 0;

		if(!vmop(state,2,&tmp,&tmp2)) {
			warnx("Out of codes?");
			return;
		}

		if(tmp == '{') {
			if(!vmop(state,2,&tmp,&tmp3)) {
				warnx("Out of codes?");
				return;
			}

			if(tmp != '}' || tmp != tmp3) {
				warnx("Unexpected input. Expected '}}', got '%c%c' instead.", (int) tmp, (int) tmp3);
				return;
			}
			tmp = tmp2;
			tmp2 = tmp3;
		}
		if(tmp2 != '}') {
			warnx("Unexpected character. Expected '}', got '%c' instead. (%ld)", (int) tmp2, tmp2);
			return;
		}

		if(!isxdigit(tmp)) {
			warnx("Unexpected character. Expected register. got '%c' instead.", (int) tmp);
			return;
		}

		tmp2 = hex2int(tmp);
		tmp = MSRG_GET(state,tmp2);
		if(tmp < 0 || tmp > state->ms_memory_size) {
			warnx("Attempted to access out of bounds memory.");
			return;
		}

		memcpy(MSRG_GETAR(state), state->ms_memory+tmp, MSRG_GETOPSZ(state,0));
		if(tmp3) MSRG_GET(state,tmp2) += MSRG_GETOPSZ(state,0);

	} else if(rN == '[') {
		tmp = varhlpr(state);
		if(tmp < 0) return;

		item = find_list_item(&state->ms_vars,state->ms_code+state->ms_ip+1,tmp);
		if(item) *(MSRG_GETAR(state)) = item->ld_value;

		state->ms_ip += (tmp+1);
	} else {
		err_unexpected_character(';', rN, state->ms_ip);
	}
}

void dumpvm(struct machinestate_s *state, long mode)
{
	register int i;
	register struct list_data_s *ldp = state->ms_vars.lc_head;
	register long j, k, l, tmp;

	switch(mode) {
		case 'a':
			printf("\n");

			__attribute__ ((fallthrough));

		case 'g':
			printf(" storage size: %ld (%lx)\n", state->ms_storage_size, state->ms_storage_size);
			printf(" memory: %p\n", state->ms_memory);
			printf(" memory size: %ld (%lx)\n", state->ms_memory_size, state->ms_memory_size);
			printf(" code: %p\n", state->ms_code);
			printf(" code size: %ld (%lx)\n", state->ms_code_size, state->ms_code_size);
			printf(" stack: %p\n", state->ms_segment.s_stack);
			printf(" stack size: %ld (%lx)\n", state->ms_stack_size, state->ms_stack_size);
			printf(" function map: %p\n", state->ms_segment.s_funcmap);
			printf(" functon map size: %ld (%lx)\n", state->ms_funcmap_size, state->ms_funcmap_size);
			printf(" op: %c\n", state->ms_code[state->ms_ip]);
			if(mode == 'g') break;

			__attribute__ ((fallthrough));

		case 'p':
			printf(" ip: %ld (%lx) (%p)\n", state->ms_ip, state->ms_ip, state->ms_code+state->ms_ip);
			printf(" mp: %ld (%lx) (%p)\n", state->ms_mp, state->ms_mp, state->ms_memory+state->ms_mp);
			printf(" rip: %ld (%lx) (%p) (%ld)\n", *(state->ms_rip), *(state->ms_rip), state->ms_rip, MSRG_GETRIPSZ(state));
			if(mode == 'p') break;

			__attribute__ ((fallthrough));

		case 'r':
			for(i = 0; i < 16; ++i) {
				printf(" r%d: %ld (%lx) (%p)\n", i, MSRG_GET(state,i),
					MSRG_GET(state,i), &(MSRG_GET(state,i)));
			}
			for(i = 0; i < 2; ++i) {
				printf(" op%d: %ld (%lx) (%p) (size: %ld)\n", i, *(MSRG_GETOP(state,i)),
					*(MSRG_GETOP(state,i)), MSRG_GETOP(state,i), MSRG_GETOPSZ(state,i));
			}
			printf(" AR: %ld (%lx) (%p) (%ld)\n", *(MSRG_GETAR(state)), *(MSRG_GETAR(state)),
				MSRG_GETAR(state), MSRG_GETCUR(state));

			if(mode == 'r') break;

			__attribute__ ((fallthrough));

		case 'v':
			for(i = 0; ldp != NULL; ldp = ldp->ld_next, ++i) {
				printf("\"%s\" = %ld; ", ldp->ld_name, ldp->ld_value);
				if(i > 0 && !(i%4)) printf("\n");
			}
			if((i%4)) printf("\n");
			break;

		case 'q':
			for(i = 0; i < 16; ++i) {
				if(i && !(i%4)) printf("\n");
				printf("r%02d=%ld, ", i, MSRG_GET(state,i));
			}
			printf("\n");
			printf("op0=%ld,op1=%ld,ar=%ld,cur=%ld\n", *(MSRG_GETOP(state,0)),
				*(MSRG_GETOP(state,1)), *(MSRG_GETAR(state)), MSRG_GETCUR(state));
			printf("mp=0x%04lx,ip=0x%04lx,sp=%p,fmp=%p\n", state->ms_mp, state->ms_ip,
				state->ms_segment.s_stack, state->ms_segment.s_funcmap);
			printf("op0sz=%ld,op1sz=%ld,rip=%p(%ld)\n", MSRG_GETOPSZ(state,0),
				MSRG_GETOPSZ(state,1), state->ms_rip, MSRG_GETRIPSZ(state));


			i = state->ms_memory[state->ms_mp];
			printf("mem[mp]=%02x ('%c')\n", i, isprint(i) ? i : '.');
			break;

		case 's':
			j = *(MSRG_GETOP(state,0));
			for(; j < state->ms_memory_size; ++j) {
				if(!state->ms_memory[j])
					break;
			}
			if(j >= state->ms_memory_size) {
				warnx("Developer error: no zero.\n");
				break;
			}
			if(!state->ms_memory[j]) {
				if(!j) break;

				if(write(STDOUT_FILENO, state->ms_memory+*(MSRG_GETOP(state,0)), j) < 0) {
					warn("write(2)");
					break;
				}

				if(state->ms_memory[j-1] != '\n')
					printf("\n");
			}
			break;

		case 'm':
			j = *(MSRG_GETOP(state,0));
			k = *(MSRG_GETOP(state,1));

			if(k < j) {
				warnx("op1 < op0, can't do this.");
				break;
			}
			else if(j < 0 || j >= state->ms_memory_size) {
				warnx("op0 out of range. (%ld)", j);
				break;
			}
			else if(k < 0 || k >= state->ms_memory_size) {
				warnx("op1 out of range. (%ld)", k);
				break;
			}

			// layout yanked from hexdump -C
			tmp = (k - j) / 16;
			if(tmp > 0) {
				while(tmp > 0) {
					printf("%08lx ", j);
					for(l = j+8; j < l; ++j) printf(" %02x", state->ms_memory[j]);

					printf(" ");
					for(l = j+8; j < l; ++j) printf(" %02x", state->ms_memory[j]);

					printf("  |");
					for(j = l-16; j < l; ++j)
						printf("%c", isprint(state->ms_memory[j]) ? state->ms_memory[j] : '.');

					printf("|\n");
					--tmp;
				}
			}

			tmp = (k - j) % 16;
			if(tmp > 0) {
				printf("%08lx ", j);

				if(tmp/8 > 0) {
					for(l = j+8; j < l; ++j) printf(" %02x", state->ms_memory[j]);
					printf(" ");
				}

				for(; j < k; ++j) printf(" %02x", state->ms_memory[j]);
				for(j=0;j<(60-(9+3*tmp+(tmp/8)));++j) printf(" ");

				printf("|");
				for(j = k-tmp; j < k; ++j) printf("%c", isprint(state->ms_memory[j]) ? state->ms_memory[j] : '.');
				printf("|\n");
			}
			break;

		default:
			if(isxdigit(mode)) {
				i = hex2int(mode);

				printf(" r%d: %ld (%lx) (%p)\n", i, MSRG_GET(state,i),
					MSRG_GET(state,i), &(MSRG_GET(state,i)));
			}
	}
}

long nextop(struct machinestate_s *state, int expect, ...)
{
	va_list ap;
	register long *op,i=1;

	if(state->ms_ip+expect+1 >= state->ms_code_size) {
		warn("Invalid opcode usage. (0x%08lx)\n", state->ms_ip);
		return 0;
	}

	va_start(ap, expect);
	for(op=va_arg(ap,long*);op!=NULL;op=va_arg(ap,long*)) {
		*op = state->ms_code[state->ms_ip+i];
		++i;
	}
	va_end(ap);

	state->ms_ip += expect;
	return expect;
}

struct machinestate_s *initvm(struct machinestate_s *state)
{
	register long memsz, stksz, fmsz;

	memset(state,0,sizeof(struct machinestate_s));

	memsz = 1024*1024*32;
	stksz = 1024*16;
	fmsz  = 1024*16;

	state->ms_storage_size = memsz;
	state->ms_memory_size = memsz - stksz - fmsz;
	state->ms_stack_size = stksz;
	state->ms_funcmap_size = fmsz;
	state->ms_reg.r_[0] = &(state->ms_reg.r_reg[14]);
	state->ms_reg.r_[1] = &(state->ms_reg.r_reg[15]);
	state->ms_reg.r_opsz[0] = sizeof(long);
	state->ms_reg.r_opsz[1] = sizeof(long);

	state->ms_memory = calloc(memsz, sizeof(char));
	if(!state->ms_memory)
		return NULL;

	state->ms_segment.s_stack = ((long *) ((char *) state->ms_memory + memsz));
	state->ms_segment.s_stack_start = ((long *) ((char *) state->ms_memory + memsz));
	state->ms_segment.s_stack_end = ((long *) ((char *) state->ms_memory + (memsz - stksz)));
	state->ms_segment.s_funcmap = ((struct function_map_s *) ((char *) state->ms_memory + (memsz - stksz)));
	state->ms_segment.s_funcmap_start = ((struct function_map_s *) ((char *) state->ms_memory + (memsz - stksz)));
	state->ms_segment.s_funcmap_end = ((struct function_map_s *) ((char *) state->ms_memory + (memsz - (stksz + fmsz))));
	state->ms_rip = state->ms_segment.s_stack_end;

	return state;
}

void destroyvm(struct machinestate_s *state)
{
	free(state->ms_memory);
	destroy_list(&state->ms_vars);
	memset(state,0,sizeof(struct machinestate_s));
}

long load_string(struct machinestate_s *state)
{
	long *ip = &(state->ms_ip), stmp;
	register long  *mp = &(state->ms_mp), hex = 0;
	register char *p, *end = state->ms_code+state->ms_code_size;
	char *start = &(state->ms_code[*ip]);

	stmp = *mp;
	for(p = start+1; p < end; ++p) {
		if(*p == '\\') {
			if(++p > end) break;

			if(*p == '"' || *p == '\\') {
				state->ms_memory[*mp] = *p;
				*mp = (*mp + sizeof(char)) % state->ms_memory_size;
				continue;
			} else if(*p == 'x') {
				hex = 0;
				++p;	// skip x.
				while(p < end && isxdigit(*p)) {
					hex = (hex * 16) + hex2int(*p);
					++p;
				}
				state->ms_memory[*mp] = hex;
				*mp = (*mp + sizeof(char)) % state->ms_memory_size;
				--p;	
				continue;
			} else if(*p == 'n') {
				state->ms_memory[*mp] = '\n';
				*mp = (*mp + sizeof(char)) % state->ms_memory_size;
				continue;
			} else {
				warnx("Uknown escape: '%c'", p[1]);
				continue;
			}
		} else if(*p == '"') {
			state->ms_memory[*mp] = 0;
			*mp = (*mp + sizeof(char));
			*(MSRG_GETOP(state,0)) = stmp;
			*(MSRG_GETOP(state,1)) = *mp;
			*(MSRG_GETAR(state)) = (*mp-1) - stmp;
			*ip += (p - start);
			return *ip;
		}
		state->ms_memory[*mp] = *p;
		*mp = (*mp + sizeof(char)) % state->ms_memory_size;
	}
	return 0;
}

long load_array(struct machinestate_s *state, long size)
{
	long *ip = &(state->ms_ip), stmp, hex = 0, mask = 0xffffffffffffffff;
	register long length = 0;
	register char *p, *end = state->ms_code+state->ms_code_size;
	char *start = &(state->ms_code[*ip]);
	int sign = 1, newvalue = 0;

	if(size == 4) mask = 0xffffffff;
	else if(size == 2) mask = 0xffff;
	else if(size == 1) mask = 0xff;
	
	stmp = state->ms_mp;
	for(p = start+1; p < end; ++p) {
		if(isxdigit(*p)) {
			hex = (hex * input_base) + hex2int(*p);
			newvalue = 1;
		} else if(*p == '-' && !newvalue) {
			sign *= -1;
		} else {
			if(newvalue) {
				hex &= mask;
				hex *= sign;

				if(state->ms_mp+size > state->ms_memory_size) {
					while(p < end && *p != ']') ++p;

					warn("out of memory");
					break;
				}

				memcpy(&state->ms_memory[state->ms_mp], &hex, size);
				state->ms_mp += size;

				length += 1;
				hex = 0;
				newvalue = 0;
			}

			if(*p == ']')
				break;
		}
	}

	state->ms_ip += (p - start);

	*(MSRG_GETOP(state,0)) = stmp;
	*(MSRG_GETOP(state,1)) = state->ms_mp;
	*(MSRG_GETAR(state)) = length;

	return 0;
}

inline int isdebughash(char *code, long ip)
{
	return (code[ip+1] == '%' && strchr("agprvqsm0123456789ABCDEF", code[ip+2]));
}


long find_matching(struct machinestate_s *state, int dir, int skip, int needle)
{
	register long count = 1, ip, size, tmp;
	register char *code = state->ms_code;

	ip = state->ms_ip;
	size = state->ms_code_size;
	while((ip >= 0 && ip < size) && count > 0) {
		if(code[ip] == '"') {
			for(ip += dir; ip >= 0 && ip < size; ip += dir) {
				if(code[ip] == '"' && code[ip+dir] != '\\')
					break;
			}
		}
		else if(code[ip] == '#' && dir > 0) {
			if((ip+2) < size && isdebughash(code,ip)) {
				ip += dir;
				continue;
			} else {
				for(ip += dir; ip < size; ip += dir) {
					if(code[ip] == '\n')
						break;
				}
			}
		}
		else if(code[ip] == '\n' && dir < 0) {
			for(tmp = ip+dir; tmp >= 0; tmp += dir) {
				if(code[tmp] == '#') {
					if(tmp+2 < size && isdebughash(code,tmp))
						continue;
					else {
						ip = tmp;
						break;
					}
				}
				else if(code[tmp] == '\n')
					break;
			}
		}
		else if(code[ip] == needle)
			--count;
		else if(code[ip] == skip)
			++count;

		ip += dir;
	}

	if(count > 0)
		return 0;

	return ip + dir;
}

void zf_write(struct machinestate_s *state)
{
	register long r1, r2, r3, *ar;

	r1 = MSRG_GET(state,1);
	r2 = MSRG_GET(state,2);
	r3 = MSRG_GET(state,3);
	ar = MSRG_GETAR(state);


	if(r2+r3 >= state->ms_memory_size) {
		warnx("attempted to reach out of bound memory");
		return;
	}

	*ar = write(r1, (char*)&state->ms_memory[r2], r3);
	if(*ar < 0) {
		warn("write(2) failed");
		*ar = 0;
	}
}

void zf_read(struct machinestate_s *state)
{
	register long r1, r2, r3, *ar;

	r1 = MSRG_GET(state,1);
	r2 = MSRG_GET(state,2);
	r3 = MSRG_GET(state,3);
	ar = MSRG_GETAR(state);

	if(r2+r3 >= state->ms_memory_size) {
		warnx("attempted to reach out of bound memory");
		return;
	}

	*ar = read(r1, (void*)&state->ms_memory[r2], r3);
	if(*ar < 0) {
		*ar = 0;
		warn("read(2) failed");
	}
}

void zf_string(struct machinestate_s *state)
{
	register long *op0, *op1, *ar;

	op0 = MSRG_GETOP(state,0);
	op1 = MSRG_GETOP(state,1);
	ar  = MSRG_GETAR(state);

	if(*op0 < 0 || *op0 >= state->ms_memory_size) {
		warnx("Attempted out of bounds memory access.");
		return;
	}

	*ar = 0;
	while(*op0 < state->ms_memory_size && state->ms_memory[*op0])
		(*ar)++, (*op0)++;

	*op1 = *op0 + 1;
}

char *to_binary(long n, int length, char *rslt)
{
	static char inbuf[65];
	register char *buf = inbuf;
	register int i;

	if(rslt) buf = rslt;

	for(i = 0; i < length; ++i) {
		buf[i] = ((n >> (length - (i+1))) & 1) ? '1' : '0';
	}
	buf[i] = '\0';

	return buf;
}

void dumpro(struct rip_object_s *ro)
{
	switch(ROFL_GETTYP(ro)) {
		case RTYP_FUNCTION:
			printf("      type: RTYP_FUNCTION\n");
			break;

		case RTYP_FORLOOP:
			printf("      type: RTYP_FORLOOP\n");
			break;

		default:
			printf("      type: UNKNOWN\n");
	}
	printf("       rip: 0x%04lx\n", ro->ro_rip);
	printf("     flags: %s (%06lx)\n", to_binary(ro->ro_flags, 24, NULL), ro->ro_flags);
	printf("     op0sz: %ld\n", ROFL_GETOPSZ(ro,0));
	printf("       op0: %ld\n", ROFL_GETOP(ro,0));
	printf("     op1sz: %ld\n", ROFL_GETOPSZ(ro,1));
	printf("       op1: %ld\n", ROFL_GETOP(ro,1));
	printf("        ar: %ld\n", ROFL_GETAR(ro));
}

void dumplo(struct loop_object_s *lo)
{
	register long op;

	op = LOFL_GETOP(lo->lo_footer.f_loop.l_break);

	printf("      type: RTYP_FORLOOP\n");
	printf("start addr: 0x%04lx\n", lo->lo_start_addr);
	printf("  end addr: 0x%04lx\n", lo->lo_end_addr);
	printf("     start: %ld\n", lo->lo_start);
	printf("   current: %ld\n", lo->lo_current);
	printf("    change: %ld\n", lo->lo_change);
	printf("       end: %ld\n", lo->lo_footer.f_loop.l_end);
	printf("     flags: %s\n", to_binary(lo->lo_footer.f_loop.l_end, 20, NULL));
	printf("        op: %c%c\n", (int) (isprint(op) ? op : op ^ '='), isprint(op) ? ' ' : '=');
}

#define stack_trace(obj, size)	(trace_stack((obj), (size), TS_ADD))
#define debug_stack		(trace_stack(NULL, 0, TS_SHOW))
void trace_stack(void *obj, long size, int opt)
{
	static char *buf = NULL, *ptr = NULL;
	static int bufsize = 4096;
	struct tracestack_object_s *to = NULL;
	register long index, max;

	switch(opt) {
		case TS_SHOW:
		{
			register long i, max;
			register char *tmp;

			if(!buf) {
				printf("No trace available.\n");
				break;
			}


			to = (struct tracestack_object_s *) buf;
			max = to->to_size;
			to = to->to_obj;
			index = 0;

			printf("Found %ld transactions.\n", max);
			for(i = 0; i < max; ++i) {
				if(to->to_size == sizeof(struct rip_object_s))
					dumpro((struct rip_object_s *) to->to_obj);
				else if(to->to_size == sizeof(struct loop_object_s))
					dumplo((struct loop_object_s *) to->to_obj);
				else
					warnx("Transactin %ld has invalid size %ld\n", i, to->to_size);

				tmp = (char *) to + sizeof(*to) + to->to_size;
				to  = (struct tracestack_object_s *) tmp;
			}
		}
		break;

		case TS_CLEANUP:
			if(buf) {
				free(buf);

				buf = NULL;
				ptr = NULL;
				bufsize = 4096;
			}
			break;


		case TS_ADD:
			if(!buf) {
				buf = calloc(size, sizeof(char));
				if(!buf) {
					warn("could not trace stack. calloc(3)");
					break;
				}

				to = (struct tracestack_object_s *) buf;
				ptr = (char *) buf + sizeof(*to);
				to->to_size = 0;
				to->to_obj = ptr;
			} else
				to = (struct tracestack_object_s *) buf;

			index = (ptr - (char *) to->to_obj);
			if(index+size > bufsize) {
				register char *tmp;
		
				bufsize += 4096;
				tmp = realloc(buf, bufsize);
				if(!tmp) {
					warn("could not trace stack. realloc(3)");
					bufsize -= 4096;
					break;
				}
		
				if(buf != tmp) {
					to = (struct tracestack_object_s *) buf;
					to->to_obj = (void *) buf + sizeof(*to);
					ptr = (char *) to->to_obj + index;
				}
			}

			++to->to_size;
			max = to->to_size;
			to = (struct tracestack_object_s *) to->to_obj;

			for(index = 0; index < max; ++index)
				to = (struct tracestack_object_s *) ((char *) to->to_obj + to->to_size + sizeof(*to));
			to = (struct tracestack_object_s *) ptr;
			to->to_size = size;
			to->to_obj = (struct tracestack_object_s *) ((char *) to + sizeof(*to));
			ptr += sizeof(*to);
			memcpy(ptr, obj, size);
			ptr += size;
			break;
	}
}

int push_rip(struct machinestate_s *state, long ip, long type)
{
	register struct loop_object_s *loopobj;
	register struct rip_object_s *ro;

	switch(type) {
		/*
		 * +-------------------------------------------------------------+
		 * | Byte N | ... |     Byte  2   |    Byte  1    |    Byte 0    |
		 * |        |     |  op0sz  op0   |  op1sz  op1   |  ar   type   |
		 * |        |     |   1111  1111  |   1111  1111  |  1111 1111   |
		 * +-------------------------------------------------------------+
		 */
		case RTYP_FUNCTION:
			if(!MSRIP_OKTOPUSH(state,RO_LENGTH)) {
				warnx("Out of stack space.");
				return 1;
			}

			ro = RO_OBJECT(state->ms_rip);

			ro->ro_rip = ip;
			ro->ro_flags = MSRG_GETOPI(state,0);
			ro->ro_flags = (ro->ro_flags << 4) | MSRG_GETOPSZ(state, 0);
			ro->ro_flags = (ro->ro_flags << 4) | MSRG_GETOPI(state,1);
			ro->ro_flags = (ro->ro_flags << 4) | MSRG_GETOPSZ(state,1);
			ro->ro_flags = (ro->ro_flags << 4) | MSRG_GETCUR(state);
			ro->ro_flags = (ro->ro_flags << 4) | type;

			//dumpro(ro);

			MSRIP_OPUSH(state, ro);
			if(gdebug) stack_trace(ro, sizeof(*ro));
			break;

		case RTYP_FORLOOP:
			if(!MSRIP_OKTOPUSH(state,LO_LENGTH)) {
				warnx("Out of stack space");
				return 1;
			}

			loopobj = LO_OBJECT((unsigned long) ip);
			loopobj->lo_footer.f_loop.l_break = (loopobj->lo_footer.f_loop.l_break << 4) | type;
			memcpy(state->ms_rip, loopobj, sizeof(*loopobj));
			MSRIP_OPUSH(state, loopobj);
			if(gdebug) stack_trace(loopobj, sizeof(*loopobj));
			break;
	}
	return 0;
}

long pop_rip(struct machinestate_s *state)
{
	register long flags, *ar, *op1, *end;
	register struct loop_object_s *loopobj;
	register struct rip_object_s *ro;
	long rip = -1;

	if(MSRIP_CHECK(state,RO_LENGTH)) {
		warnx("No stack.. Not in {}?");
		return rip;
	}

	MSRIP_POP(state, RO_LENGTH);
	ro = (struct rip_object_s *) state->ms_rip;
	flags = ro->ro_flags;
	switch((flags & 0xf)) {
		case RTYP_FUNCTION:
			flags >>= 4;
			MSRG_GETCUR(state) = (flags & 0xf);

			flags >>= 4;
			MSRG_GETOPSZ(state,1) = (flags & 0xf);

			flags >>= 4;
			MSRG_GETOP(state,1) = &(MSRG_GET(state, (flags & 0xf)));

			flags >>= 4;
			MSRG_GETOPSZ(state,0) = (flags & 0xf);

			flags >>= 4;
			MSRG_GETOP(state,0) = &(MSRG_GET(state, (flags & 0xf)));

			//dumpro(ro);

			rip = ro->ro_rip;
			break;

		case RTYP_FORLOOP:
			if(MSRIP_CHECK(state, LO_SIZE)) {
				warnx("Out of stack? How'd this happen?");
				break;
			}

			MSRIP_POP(state, LO_SIZE);

			loopobj = (struct loop_object_s *) state->ms_rip;

			ar = MSRG_GETAR(state);
			op1 = MSRG_GETOP(state,1);
			*ar = loopobj->lo_current;
			*op1 = loopobj->lo_change;
			end = &loopobj->lo_footer.f_loop.l_end;

			/*if(LOFL_GETREG(loopobj->lo_footer.f_loop.l_break))
				*end = MSRG_GET(state, LOFL_GETREG(loopobj->lo_footer.f_loop.l_break)-1);*/

			*(MSRG_GETOP(state,0)) = loopobj->lo_start;

#define loophlpr(flag, op, chgop) \
			case flag: \
				   if(*ar op *end) { \
					   loopobj->lo_current chgop *op1; \
					   MSRIP_PUSH(state, LO_LENGTH); \
					   rip = loopobj->lo_start_addr; \
				   } else rip = loopobj->lo_end_addr; \
				   break;

			switch(LOFL_GETOP(loopobj->lo_footer.f_loop.l_break)) {
				loophlpr('<', <, +=);
				loophlpr('>', >, -=);
				loophlpr('=', ==, +=);
				loophlpr(0, ==, -=);	/* '=' ^ '=' */
				loophlpr(28, !=, +=);	/* '!' ^ '=' */
				loophlpr(3, >=, -=);	/* '>' ^ '=' */
				loophlpr(1, <=, +=);	/* '<' ^ '=' */
			}
			

			break;
#undef loophlpr
	}

	return rip;
}

long call_function(struct machinestate_s *state)
{
	register long *ar = MSRG_GETAR(state);
	struct core_functions_s core[] = {
		{ 0, NULL },
		{ 1, zf_write },	/* write function:
					 *     AR = 1
					 *     r1 = fd
					 *     r2 = mem address
					 *     r3 = size
					 */

		{ 2, zf_read },		/* read function:
					 *     AR = 2
					 *     r1 = fd
					 *     r2 = mem address
					 *     r3 = size
					 */

		{ 3, zf_string },	/* string function:
					 *     AR = 3
					 *     op0 = memory location of string
					 */
	};
	register long size = sizeof(core)/sizeof(core[0]);
	register struct function_map_s *fmp;

	if(*ar < size) {
		core[*ar].func(state);
		return 0;
	}

	for(fmp = state->ms_segment.s_funcmap; fmp < state->ms_segment.s_funcmap_start; ++fmp) {
		if(fmp->fm_fn == *ar) {
			if(!push_rip(state, state->ms_ip, RTYP_FUNCTION)) {
				state->ms_ip = fmp->fm_addr;
				return 0;
			} else {
				warnx("Could not push RIP. Out of stack space.");
				warnx("Not calling function %ld (%lx) at ip %lx",
					*ar, *ar, state->ms_ip);
			}
		}
	}

	warnx("Function %ld doesn't exist.", *(MSRG_GETAR(state)));
	return 0;
}

struct function_map_s *find_function(struct machinestate_s *state, long fn)
{
	register struct function_map_s *fmp = NULL;

	for(fmp = state->ms_segment.s_funcmap; fmp < state->ms_segment.s_funcmap_start; ++fmp) {
		if(fmp->fm_fn == fn) {
			return fmp;
		}
	}

	return NULL;
}

int runvm(struct machinestate_s *state)
{
	register char *code;
	register long *ip, hex=0, max, line = 1, column = 0;
	long *ar, tmp, arg[4] = {0};
	int rc = 0, sign = 1;

	code = state->ms_code;
	ip = &(state->ms_ip);
	ar = &(state->ms_reg.r_reg[state->ms_reg.r_ar]);
	max = state->ms_code_size;

	while(*ip < max) {
		++column;

		if(isxdigit(code[*ip])) {
			hex = (hex * input_base) + hex2int(code[*ip]);
			*ar = hex * sign;
			(*ip)++;
			continue;
		} else if(hex != 0) {
			*ar = hex * sign;
			hex = 0;
			sign = 1;
		} else if(hex == 0 && code[*ip] == '-' && isxdigit(MSIP_PEEK(state,*ip+1))) {
			sign = -1;
			*ip += 1;
			continue;
		}

		switch(code[*ip]) {
			case '\n':
				LINE_CHANGE(line, column);
				break;

			case ':':
				if(!vmop(state, 1, &tmp)) {
					err_need_args(':', *ip);
					break;
				}

				i_cpy1ar(state, tmp);
				break;

			case ';':
				if(!vmop(state, 1, &tmp)) {
					err_need_args(';', *ip);
					break;
				}

				i_cpy2ar(state, tmp);
				break;

			case '[':
				load_array(state, state->ms_reg.r_opsz[0]);
				break;

			case ']':
				warnx("Unbalanced brackets on line %ld.", line);
				break;

			case '=':
				*ar = *(MSRG_GETOP(state,0)) ^ *(MSRG_GETOP(state,1));
				break;

			case '^':
				if(state->ms_segment.s_stack > state->ms_segment.s_stack_end) {
					--state->ms_segment.s_stack;
					*(state->ms_segment.s_stack) = *ar;
				}
				break;

			case 'v':
				if(state->ms_segment.s_stack < state->ms_segment.s_stack_start ) {
					*ar = *(state->ms_segment.s_stack);
					++state->ms_segment.s_stack;
				}
				break;

			case '"':
				if(!load_string(state))
					warnx("Failed to load to string.");

				break;

			case 'q':
			{
				register long *src, *dst;
				if(!vmop(state, 2, &arg[0], &arg[1])) {
					err_need_args('q', *ip);
					break;
				}

				if(isxdigit(arg[0])) src = &state->ms_reg.r_reg[hex2int(arg[0])];
				else if(arg[0] == 'm') src = &state->ms_mp;
				else if(arg[0] == ':') src = MSRG_GETOP(state,0);
				else if(arg[0] == ';') src = MSRG_GETOP(state,1);
				else if(arg[0] == '_') src = MSRG_GETAR(state);
				else {
					err_unexpected_character('q', arg[0], *ip);
					break;
				}

				if(isxdigit(arg[1])) dst = &state->ms_reg.r_reg[hex2int(arg[1])];
				else if(arg[1] == 'm') dst = &state->ms_mp;
				else if(arg[1] == ':') dst = MSRG_GETOP(state,0);
				else if(arg[1] == ';') dst = MSRG_GETOP(state,1);
				else if(arg[1] == '_') dst = MSRG_GETAR(state);
				else {
					err_unexpected_character('q', arg[1], *ip);
					break;
				}

				*dst = *src;
			}
			break;

			case '?':
			{
				register long *op0, *op1, jmpaddr;

				if(!vmop(state, 2, &arg[0], &arg[1])) {
					err_need_args('?', *ip);
					break;
				}

				if(arg[0] != '(') {
					err_unexpected_character('?', arg[0], *ip);
					break;
				}

				tmp = find_matching(state, 1, '(', ')');
				if(!tmp) {
					err_need_args('(', *ip);
					break;
				}

				// encountered else
				if(code[tmp-1] == '(') jmpaddr = tmp-1;
				else jmpaddr = tmp-3;

				op0 = MSRG_GETOP(state,0);
				op1 = MSRG_GETOP(state,1);

#define cmphelper(ch, cmp) 	case ch: \
					if(!(cmp)) *ip = jmpaddr; \
					break;

				switch(arg[1]) {
					cmphelper('!', !*ar);
					cmphelper('?', *ar);
					cmphelper('<', *op0 < *op1);
					cmphelper('>', *op0 > *op1);
					cmphelper('=', *op0 == *op1);
					cmphelper('/', *op0 != *op1);
					cmphelper(']', *op0 >= *op1);
					cmphelper('[', *op0 <= *op1);

					default:
						warnx("Unexpected character '%c'", safe_print(arg[1]));
				}
#undef cmphelper
			}
			break;

			case ')':
			{
				register long *op0, *op1;

				if(!vmop(state, 1, &arg[0])) {
					err_need_args(')', *ip);
					break;
				}

				if(arg[0] == ';')
					break;	/* we're done here. */

				*ip -= 2;
				tmp  = find_matching(state, -1, ')', '(');
				*ip += 2;

				if(!tmp) {
					warnx("Error: mismatched ')'");
					break;
				}

				op0 = MSRG_GETOP(state,0);
				op1 = MSRG_GETOP(state,1);

				switch(arg[0]) {
					case '?': if(*ar) *ip = tmp+3; break;
					case '!': if(!*ar) *ip = tmp+3; break;
					case '<': if(*op0 < *op1) *ip = tmp+3; break;
					case '>': if(*op0 > *op1) *ip = tmp+3; break;
					case '=': if(*op0 == *op1) *ip = tmp+3; break;
					case '/': if(*op0 != *op1) *ip = tmp+3; break;
					case ']': if(*op0 > *op1) *ip = tmp+3; break;
					case '[': if(*op0 < *op1) *ip = tmp+3; break;
					case '(': 
						*ip += 2;
						arg[1] = find_matching(state, 1, '(', ')');
						if(!arg[1]) {
							warnx("Error: mismatched ')('");
							*ip -= 2;
							break;
						}
						*ip = arg[1];
						break;
				}
			}
			break;

			case '_':
				if(!vmop(state, 1, &tmp)) {
					err_need_args('_', *ip);
					break;
				}

				if(isxdigit(tmp)) {
					tmp = hex2int(tmp);
					state->ms_reg.r_ar = tmp;
					ar = &(state->ms_reg.r_reg[tmp]);
				} else if(tmp == ':') {
					ar = state->ms_reg.r_[0];
					tmp = (long) ((unsigned long) (ar - &state->ms_reg.r_reg[0]));
					state->ms_reg.r_ar = tmp;
				} else if(tmp == ';') {
					ar = state->ms_reg.r_[1];
					tmp = (long) ((unsigned long) (ar - &state->ms_reg.r_reg[0]));
					state->ms_reg.r_ar = tmp;
				} else if(tmp == '_') {
					if(*ar < 16) {
						tmp = *ar;
						ar = &state->ms_reg.r_reg[tmp];
						state->ms_reg.r_ar = tmp;
					} else if(*ar == 17 || *ar == 18) {
						ar = state->ms_reg.r_[*ar % 2];
						tmp = (long) ((unsigned long) (ar - &state->ms_reg.r_reg[0]));
						state->ms_reg.r_ar = tmp;
					} else {
						warnx("Register out of range.");
						break;
					}
				} else {
					err_unexpected_character('_', tmp, *ip);
				}
				break;

			case '~':
				if(!vmop(state, 1, &arg[0])) {
					warnx("Expected argument to '~' operator.");
					break;
				}

				if(isxdigit(arg[0])) {
					if(!vmop(state, 1, &arg[1])) {
						warnx("Expected argument to '~' operator.");
						break;
					}

					if(!isxdigit(arg[1])) {
						warnx("Expected hexadecimal input to '~' operator.");
						break;
					}

					state->ms_reg.r_[0] = &(state->ms_reg.r_reg[hex2int(arg[0])]);
					state->ms_reg.r_[1] = &(state->ms_reg.r_reg[hex2int(arg[1])]);

				}
				else if(arg[0] == '~') {
					if(!vmop(state, 1, &arg[1])) {
						warnx("Expected argument to '~~' operator.");
						break;
					}

					if(strchr("1248", arg[1])) {
						state->ms_reg.r_opsz[*ar%2] = hex2int(arg[1]);
						break;
					}

					switch(arg[1]) {
						case '_':
							arg[2] = *(MSRG_GETOP(state,0));
							arg[3] = *(MSRG_GETOP(state,1));

							if(arg[2] > 15) warnx("Invalid op setting in ~~_ for op0");
							else MSRG_GETOP(state,0) = &(MSRG_GET(state,arg[2]));

							if(arg[3] > 15) warnx("Invalid op setting in ~~_ for op1");
							else MSRG_GETOP(state,1) = &(MSRG_GET(state,arg[3]));
							break;

						case '~':
							*ar = ophlpr(state, arg[1]);
							break;

						case '.':
						{
							register long *op0;

							op0 = MSRG_GETOP(state, 0);
							if(*op0 < 0 || (*op0 + *ar) >= state->ms_memory_size) {
								warnx("Memory address out of range in ~~.");
								break;
							}

							if(write(STDOUT_FILENO, &state->ms_memory[*op0], *ar) < 0)
								warn("write(2) failed");
						}
						break;

						case ',':
						{
							register long *op0;

							op0 = MSRG_GETOP(state, 0);
							if(*op0 < 0 || (*op0 + *ar) >= state->ms_memory_size) {
								warnx("Memory address out of range for ~~,");
								break;
							}

							if(read(STDIN_FILENO, &state->ms_memory[*op0], *ar) < 0)
								warn("read(2) failed");
						}
						break;

						default:
							warnx("Unknown finishing operator '%c' for '~~' operator.", safe_print(arg[1]));
					}
					break;
				}

				switch(arg[0]) {
					case '.':
						if(input_base == 16) printf("%lx", *ar);
						else printf("%ld", *ar);
						fflush(stdout);
						break;

					case ',':
						if(input_base == 16) {
							if(scanf("%lx", &arg[1]) == 1) *ar = arg[1];
							else *ar = 0;
						} else {
							if(scanf("%ld", &arg[1]) == 1) *ar = arg[1];
							else *ar = 0;
						}
						break;
				}
				break;

			case '#':
				if(!vmop(state, 2, &arg[0], &arg[1]))
					break;

				if(arg[0] == '%') {
					dumpvm(state, arg[1]);
					break;
				} else if(arg[0] == '\n') {
					LINE_CHANGE(line, column);
					*ip -= 2;
					break;
				} else if(arg[1] == '\n') {
					LINE_CHANGE(line, column);
					--(*ip);
					break;
				}

				while(*ip < max && code[*ip] != '\n')
					(*ip)++;

				LINE_CHANGE(line, column);
				break;

			case '+':
			case '-':
			case '*':
			case '/':
			case '%':
			case '&':
			case '|':
			case '>':
			case '<':
				*ar = ophlpr(state, code[*ip]);
				break;

			case 'S':
			case '$':
			case 'T':
				arg[0] = code[*ip];

				if(!vmop(state, 1, &tmp)) {
					err_need_args('S', *ip);
					break;
				}

				if(arg[0] == '$' && tmp != '[') {
					memhlpr(state, tmp);
					break;
				}

				if(tmp != '[') {
					err_unexpected_character('S', tmp, *ip);
					break;
				}

				tmp = varhlpr(state);
				if(tmp < 0) break;
				switch(arg[0]) {
					case 'S':
						update_list_item(&state->ms_vars, code+*ip+1, tmp, state->ms_mp);
						break;

					case '$':
					{
						register struct list_data_s *item;

						item = find_list_item(&state->ms_vars, code+*ip+1, tmp);
						if(item) {
							if(item->ld_value >= state->ms_memory_size) {
								warnx("Attempted to memory to index out of bounds");
								break;
							}
							state->ms_mp = item->ld_value;
						}
					}
					break;

					case 'T':
						drop_list_item(&state->ms_vars, code+*ip+1, tmp);
						break;
				}

				*ip += (tmp+1);
				break;

			case '.':
				if(write(STDOUT_FILENO, ar, state->ms_reg.r_opsz[0]) <= 0)
					warn("write(2)");
				break;

			case ',':
				*ar = 0;
				if(read(STDIN_FILENO, ar, state->ms_reg.r_opsz[0]) <= 0)
					warn("read(2)");
				break;

			case '\'':
				if(!vmop(state, 2, &arg[0], &arg[1])) {
					warnx("Expected arguments to '\''.");
					break;
				}

				if(arg[0] == '\\') {
					arg[0] = arg[1];
					if(!vmop(state, 1, &arg[1])) {
						warnx("Expected arguments to '\''.");
						break;
					}

					switch(arg[0]) {
						case  'n': tmp = '\n'; break;
						case  'r': tmp = '\r'; break;
						case '\\': tmp = '\\'; break;
						case '\'': tmp = '\''; break;
						case  'x':
							   tmp = 0;
							   while(isxdigit(arg[1])) {
								   tmp = (tmp * 16) + hex2int(arg[1]);
								   if(!vmop(state,1,&arg[1])) {
									warnx("Expected arguments to '\''.");
									break;
								   }
							   }
							   break;

						default:
							   warnx("Unknown escape character: '%c'", (int) arg[0]);
					}
				} else
					tmp = arg[0];

				if(arg[1] != '\'') {
					warnx("Error: expected character '\'', but got '%c' instead.", (int) arg[1]);
					break;
				}
				*ar = tmp;
				break;

			case '`':
				tmp = *ip+1;
				arg[0] = 0;
				while(arg[0] != '`') {
					if(!vmop(state,1,&arg[0])) {
						warnx("Error: missing closing '`'.");
						break;
					}

					if(arg[0] == '\\' && MSIP_PEEK(state,*ip+1) == '`')
						*ip += 2;
				}

				// sanity check.
				if(arg[0] != '`')
					break;

				arg[1] = *ip - tmp;	// string length
				if(arg[1] > (signed) sizeof(*ar)) {
					warnx("Error: String is too long to load into %lu bytes", sizeof(*ar));
					break;
				}

				*ar = 0;
				memcpy(ar, code+tmp, arg[1]);
				break;

			case '@':
				if(*ar == 0) {
					rc = (int) *(MSRG_GETOP(state,0));
					goto _out;
				}
				call_function(state);
				break;

			case '{':
			{
				register struct function_map_s *fmp;

				*ip += 1;
				tmp = find_matching(state,1,'{','}');
				if(!tmp) {
					warnx("Unbalanced function.");
					break;
				}

				if(*ar < 48) {
					warnx("Attempted to define a core function.");
					*ip = tmp;
					break;
				}

				fmp = find_function(state, *ar);
				if(fmp) fmp->fm_addr = *ip-1;
				else if(state->ms_segment.s_funcmap > state->ms_segment.s_funcmap_end) {
					--state->ms_segment.s_funcmap;
					state->ms_segment.s_funcmap->fm_fn = *(MSRG_GETAR(state));
					state->ms_segment.s_funcmap->fm_addr = *ip-1;
				}

				*ip = tmp-1;
			}
			break;

			case '}':
				tmp = pop_rip(state);
				if(tmp) *ip = tmp;
				break;

			case '!':
				tmp = MSIP_PEEK(state, *ip+1);
				if(tmp < 0) {
					warnx("Hanging '!'");
					break;
				}

				switch(tmp) {
					case '!':
						*ip += 1;
						if(!vmop(state, 2, &arg[0], &arg[1])) {
							warnx("Expected arguments to '!!' line %ld, byte %ld", line, *ip);
							break;
						}

						if(!isxdigit(arg[0])) {
							warnx("Expected digit value for '!!' on line %ld, byte %ld (got '%c')",
								line, *ip, (int) arg[0]);
							break;
						}
						else if(!isxdigit(arg[1])) {
							warnx("Expected digit value for '!!' on line %ld, byte %ld (got '%c')",
								line, *ip, (int) arg[1]);
							break;
						}

						arg[0] = hex2int(arg[0]);
						arg[1] = hex2int(arg[1]);

						input_base = (arg[0] * input_base) + arg[1];
						break;

					case '(':
					{
						struct loop_object_s loopobj;
						register long isptr = 0, end = 0;

						*ip += 2;
						tmp = find_matching(state, 1, '(', ')');
						if(!tmp) {
							warnx("Could not find closing ')' to opening '!(' (line %ld)", line);
							return 1;
							break;
						}

						if(MSIP_PEEK(state, tmp-1) != '{') {
							warnx("Expected '!(){' (line %ld)", line);
							break;
						}

						// get comparison
						*ip -= 1;
						if(!vmop(state, 1, &arg[0])) {
							warnx("Expected more parameters to '!('. (line %ld)", line);
							break;
						}

						if(MSIP_PEEK(state, *ip+1) == '=') {
							*ip += 1;
							arg[0] ^= '=';
						}
						

						if(!vmop(state, 1, &arg[1])) {
							warnx("Expected more parameters to '!('. (line %ld)", line);
							break;
						}

						if(arg[1] == ';') {
							if(!vmop(state, 3, &arg[1], &arg[2], &arg[3])) {
								warnx("Expected more parameters to '!('. (line %ld)", line);
								break;
							}

							/* should be ;(r) */
							if(arg[1] == '(') {
								arg[1] = arg[2];
								arg[2] = arg[3];
								if(!vmop(state, 1, &arg[3])) {
									warnx("Expected more parameters to '!('. (line %ld)", line);
									break;
								}
								isptr = hex2int(arg[1]);
							}

							if(!isxdigit(arg[1])) {
								warnx("Invalid parameter to '!(', expected register, got '%c'", (int) arg[1]);
								break;
							}

							arg[1] = MSRG_GET(state, hex2int(arg[1]));
						} else if(!isxdigit(arg[1])) {
							warnx("Invalid parameters to '!(', expected number, got '%c'", (int) arg[1]);
							break;
						} else { // input
							end = hex2int(arg[1]);
							while(isxdigit(arg[1])) {
								if(!vmop(state, 1, &arg[1])) {
									warnx("Expected more parameters to '!('. (line %ld)", line);
									break;
								}

								if(isxdigit(arg[1])) {
									end = (end * input_base) + hex2int(arg[1]);
								} else {
									// arg[1] is probably ')'
									if(!vmop(state, 1, &arg[3])) {
										warnx("Expected more parameters to '!('. (line %ld)", line);
										break;
									}

									arg[2] = arg[1];
								}
							}
							arg[1] = end;
						}

						if(arg[2] != ')' || arg[3] != '{') {
							warnx("Invalid parameters to '!(', expected '){' got '%c%c'\n",
								(int) arg[2], (int) arg[3]);
							break;
						}

						*ip += 1;
						tmp = find_matching(state, 1, '{', '}');
						if(!tmp) {
							warnx("Could not find closing loop.");
							break;
						}

						if(arg[0] && !strchr("<=>!\x03\x01\x1c", arg[0])) {
							warnx("Invalid comparison operator in '!(', got '%c'", (int) arg[0]);
							break;
						}

						*ar = *(MSRG_GETOP(state,0));

						loopobj.lo_start_addr = *ip-1;
						loopobj.lo_end_addr = tmp-2;
						loopobj.lo_change = *(MSRG_GETOP(state,1));
						loopobj.lo_current = *ar;
						loopobj.lo_start = *(MSRG_GETOP(state,0));
						loopobj.lo_footer.f_loop.l_end = arg[1];
						loopobj.lo_footer.f_loop.l_break = (arg[0] << 8) | (isptr + 1);

						push_rip(state, (long) (unsigned long) &loopobj, RTYP_FORLOOP);
						tmp = pop_rip(state);
						if(tmp >= 0) *ip = tmp;
					}
					break;
				}
				break;
		}
		(*ip)++;
	}

_out:
	return rc;
}

int main(int argc, char *argv[])
{
	register int rc, fd; 
	struct stat fdinfo;
	struct machinestate_s state;
	setlocale(LC_ALL, "");

	if(argc < 2) {
		printf("Usage: %s <source>\n", argv[0]);
		return 1;
	}

	fd = open(argv[1], O_RDONLY);
	if(fd < 0) {
		err(errno, "%s: Failed to open %s\n", argv[0], argv[1]);
	}

	rc = fstat(fd, &fdinfo);
	if(rc < 0) {
		close(fd);
		err(errno, "%s: Failed to stat %s (was %d)\n", argv[0], argv[1], fd);
		//exit(errno);
	}

	if(!initvm(&state)) {
		close(fd);
		err(errno,"initvm()");
		//exit(errno);
	}

	state.ms_code_size = fdinfo.st_size;
	state.ms_code = mmap(NULL, fdinfo.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if(!state.ms_code) {
		destroyvm(&state);
		close(fd);

		err(errno,"mmap(2)");
	}

	rc = runvm(&state);

	trace_stack(NULL,0,TS_CLEANUP);
	munmap(state.ms_code, state.ms_code_size);
	close(fd);

	return rc;
}
