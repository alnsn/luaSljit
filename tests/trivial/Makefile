PROG=	t_trivial
SRCS=	t_trivial.c

PKG_CONFIG_DEPS+=	lua

WARNS=	4

COPTS+=		-O2 -g
CPPFLAGS+=	-DSLJIT_CONFIG_AUTO=1
CPPFLAGS+=	-DSLJIT_VERBOSE=0
CPPFLAGS+=	-DSLJIT_DEBUG=0
CPPFLAGS+=	-I ../../sljit/sljit_src/
CPPFLAGS+=	-I ../../lib

LDADD+=		-lluaSljit -lsljit
LDFLAGS+=	-L ${.OBJDIR}/../../lib
LDFLAGS+=	-L ${.OBJDIR}/../../sljit/sljit_src

.include <mkc.prog.mk>
