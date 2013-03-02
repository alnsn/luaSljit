LUA_CMODULE=	sljit.init
SRCS=		sljit.c

WARNS=	4

COPTS+=		-O2 -g
CPPFLAGS+=	-DSLJIT_CONFIG_AUTO=1
CPPFLAGS+=	-DSLJIT_VERBOSE=0
CPPFLAGS+=	-DSLJIT_DEBUG=0
CPPFLAGS+=	-I ../sljit/sljit_src/

LDADD+=		-lsljit
LDFLAGS+=	-L ${.OBJDIR}/../sljit/sljit_src

.include <mkc.lib.mk>
