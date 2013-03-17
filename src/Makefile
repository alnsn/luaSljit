LIB=	luaSljit
SRCS=	luaSljit.c

PKG_CONFIG_DEPS+=	lua

SHLIB_MAJOR=	1
SHLIB_MINOR=	0
SHLIB_TEENY=	0

LDCOMPILER=	yes

WARNS=	4

COPTS+=		-O2 -g
CPPFLAGS+=	-DSLJIT_CONFIG_AUTO=1
CPPFLAGS+=	-DSLJIT_VERBOSE=0
CPPFLAGS+=	-DSLJIT_DEBUG=0
CPPFLAGS+=	-I ../sljit/sljit_src/

LDADD+=		-lsljit
LDFLAGS+=	-L ${.OBJDIR}/../sljit/sljit_src

.include <mkc.lib.mk>
