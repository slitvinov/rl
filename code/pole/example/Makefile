.POSIX:
.SUFFIXES: .c .o .a60
.PHONY: all lint clean

M_LDFLAGS = -static  -L../lib -lpole -lm
M_CFLAGS = -I../lib

CFLAGS = -Og -g
LINK = $(CC)
MARST = marst
M = \
rhs0 \

all: $M
.o:; $(LINK) $< $(M_LDFLAGS) $(LDFLAGS) -o $@
.c.o:; $(CC) $(M_CFLAGS) $(CFLAGS) $< -c
.c:
%: %.c

lint:; make CFLAGS='-Wall -Wextra -g -Og'
clean:; rm -f $M $(M:=.o)
