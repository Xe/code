AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/libratbox/include @PY_CFLAGS@
AM_LDFLAGS = -Wl,-export-dynamic @PY_LIBS@
LDADD = -ldl

bin_PROGRAMS = ircd
ircd_SOURCES = main.c\
	state.c \
	noddy.c
