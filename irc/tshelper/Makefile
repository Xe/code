# Simple Makefile to tshelper

SRCS = tshelper.c
OBJS = ${SRCS:.c=.o}

CC = gcc
CFLAGS = -O2 -g -Wall
LDFLAGS =

.c.o: Makefile
	$(CC) $(CFLAGS) -c $<

tshelper: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)
