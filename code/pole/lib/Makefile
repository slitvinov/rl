.POSIX:
.SUFFIXES: .c .o
.PHONY: lint clean

CFLAGS = -Og -g
L = libpole.a
O = \
pole.o

$L: $O; ar r $@ $O && ranlib $@
.c.o:; $(CC) $(CFLAGS) $< -c

pole.o: pole.h

.c:
%: %.c

lint:; make CFLAGS='-Wall -Wextra -g -O2'
clean:; rm -f $L $O
