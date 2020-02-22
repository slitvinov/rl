.POSIX:
M = \
pole\

LINK = $(CC)
M_LDFLAGS = -lm

all: $M
.o:
	$(LINK) $< $(LDFLAGS) $(M_LDFLAGS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -c -o $@

.c:
%: %.c

lint:; make CFLAGS='-Wall -Wextra -g -O2'

.PHONY: clean lint
clean:; rm -f $M $(M:=.o)
