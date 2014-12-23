Lua sljit library
=================

Prerequisites
-------------

You need sljit (http://sljit.sourceforge.net/) and mk-configure
(http://sourceforge.net/projects/mk-configure/) to build luaSljit.

It is recommended to use pkgsrc and install devel/mk-configure.
The pkgsrc guide is available at http://www.netbsd.org/docs/pkgsrc/.

The sljit code must be at revision r291 or newer:

	$ svn co https://svn.code.sf.net/p/sljit/code@r291 sljit

	$ tar cf sljit-r291.tar sljit/

Building
--------

Extract sljit tarball to sljit/ subdirectory. Make sure you pass
--keep-old-files (-k) option to tar to keep Makefiles from luaSljit.

	$ cd sljit/

	$ tar kxf /path/to/sljit-r291.tar

Then you can build luaSljit with this command

	$ mkcmake

and install:

	$ export DESTDIR=/path/of/your/choice

	$ env PREFIX=/ mkcmake install

TODO
----

	-- Boost code generation performance with LuaJIT
	local sljit = require 'sljit.turbo'
