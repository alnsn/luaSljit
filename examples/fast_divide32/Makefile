PROG=	fast_divide32
SRCS=	fast_divide32.c

PKG_CONFIG_DEPS+=	lua

LDCOMPILER=	yes

WARNS=	4

COPTS+=		-O2 -g -march=core2
CPPFLAGS+=	-DSLJIT_CONFIG_AUTO=1
CPPFLAGS+=	-DSLJIT_VERBOSE=0
CPPFLAGS+=	-DSLJIT_DEBUG=0
CPPFLAGS+=	-I ../../lib
CPPFLAGS+=	-I ../../sljit/sljit_src/

LDADD+=		-lluaSljit -lsljit
LDFLAGS+=	-L ${.OBJDIR}/../../lib
LDFLAGS+=	-L ${.OBJDIR}/../../sljit/sljit_src

.include <mkc.prog.mk>
