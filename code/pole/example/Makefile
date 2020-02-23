.POSIX:
.SUFFIXES: .c .o
.PHONY: all lint clean

GSL_LDFLAGS = `gsl-config --libs`

M_LDFLAGS = -L../lib -lpole -lm
M_CFLAGS = -I../lib

CFLAGS = -Og -g
LINK = $(CC)
MARST = marst
M = \
ode\
rhs\
rhs0\
traj\

all: $M
.o:; $(LINK) $< $(M_LDFLAGS) $(GSL_LDFLAGS) $(LDFLAGS) -o $@
.c.o:; $(CC) $(M_CFLAGS) $(CFLAGS) $< -c
.c:
%: %.c

lint:; make CFLAGS='-Wall -Wextra -g -Og'
clean:; rm -f $M $(M:=.o)
