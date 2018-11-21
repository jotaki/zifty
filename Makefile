CC= gcc
CFLAGS= -W -Wall -g -O2
SRCS= zifty.c
OBJS= $(SRCS:.c=.o)

all: zifty

$(OBJS): $(SRCS)

zifty: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

clean:
	rm -f zifty $(OBJS)
