
####
#### Copyright (C) 2015 David Pirotte
#### David Pirotte <david at altosw dot be>

#### This file is part of Grip.
#### A Grip of Really Important Procedures.

#### Grip is free software: you can redistribute it and/or modify it
#### under the terms of the GNU General Public License as published by
#### the Free Software Foundation, either version 3 of the License, or
#### (at your option) any later version.

#### Grip is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with Grip.  If not, see <http://www.gnu.org/licenses/>.
####


include $(top_srcdir)/am/guile.mk

moddir=$(gglobalsitedir)
godir=$(gsiteccache)

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=meta/grip-1.0.pc

SOURCES = \
	grip/reexport.scm		\
	grip/g-export.scm		\
	grip/goops.scm			\
	grip/passwd.scm			\
	grip/config.scm			\
	grip/path.scm			\
	grip/do.scm			\
	grip/push.scm			\
	grip/swap.scm			\
	grip/regexp.scm			\
	grip/i18n.scm			\
	grip/utils.scm			\
	grip/locale.scm			\
	grip/strings.scm		\
	grip/lists.scm			\
	grip/keyword.scm		\
	grip/server.scm			\
	grip/dates.scm			\
	grip/nbs/fp.scm			\
	grip/nbs/hex.scm		\
	grip/nbs/nb2str.scm		\
	grip/nbs/str2nb.scm		\
	grip/nbs/pi.scm			\
	grip/nbs.scm			\
	grip/xft.scm			\
	grip/m4/support.scm		\
	grip/tex-utils.scm		\
	grip/support/slib.scm		\
	grip/fs-ops.scm

NOCOMP_SOURCES = \
	grip/support/glob.scm		\
	grip/m4/guile-gnome-2.m4	\
	grip/m4/guile.m4		\
	grip/m4/latex.m4

if INSTALL_GRIP_GNOME
SOURCES += \
	grip/gnome/colours.scm		\
	grip/gnome/filechooser.scm	\
	grip/gnome/key-press.scm	\
	grip/gnome/gtk2.scm		\
	grip/gnome/mdialog-1b.scm	\
	grip/gnome/mdialog-2b.scm	\
	grip/gnome/mdialogs.scm		\
	grip/gnome.scm
NOCOMP_SOURCES += \
	grip/gnome/colour-sets		\
	$(shell find $(top_srcdir)/grip/gnome/glade -type f)
endif

if INSTALL_GRIP_CLUTTER
SOURCES += \
	grip/clutter/utils.scm			\
	grip/clutter/colour.scm			\
	grip/clutter/globals.scm		\
	grip/clutter/animation.scm		\
	grip/clutter/align.scm			\
	grip/clutter/constraint.scm		\
	grip/clutter/gravity.scm		\
	grip/clutter/orientation.scm		\
	grip/clutter/stage.scm			\
	grip/clutter/actor.scm			\
	grip/clutter/image.scm			\
	grip/clutter/label.scm			\
	grip/clutter/apple.scm			\
	grip/clutter/bin.scm			\
	grip/clutter/box.scm			\
	grip/clutter/grid.scm			\
	grip/clutter/text.scm			\
	grip/clutter/tooltip.scm		\
	grip/clutter/toolbar-item.scm		\
	grip/clutter/toolbar.scm		\
	grip/clutter/bouncer.scm		\
	grip/clutter/clock.scm			\
	grip/clutter/drag.scm			\
	grip/clutter/drop.scm			\
	grip/clutter.scm			\
	grip/clutter/examples/clue.scm		\
	grip/clutter/examples/utils.scm		\
	grip/clutter/examples/bouncer.scm	\
	grip/clutter/examples/grid.scm		\
	grip/clutter/examples/spline.scm	\
	grip/clutter/examples/toolbar.scm	\
	grip/clutter/examples/scroll.scm	\
	grip/clutter/examples/drag.scm		\
	grip/clutter/examples/drop.scm

NOCOMP_SOURCES += \
	$(shell find $(top_srcdir)/grip/clutter/icons -type f)
GSCRIPTS = \
	grip/clutter/examples/template		\
	grip/clutter/examples/bouncer		\
	grip/clutter/examples/clock		\
	grip/clutter/examples/grid		\
	grip/clutter/examples/spline		\
	grip/clutter/examples/toolbar		\
	grip/clutter/examples/scroll		\
	grip/clutter/examples/drag		\
	grip/clutter/examples/drop
endif

if INSTALL_GRIP_SQLITE
SOURCES += \
	grip/sqlite3.scm		\
	grip/db/filters.scm		\
	grip/db/sqlite3.scm		\
	grip/db/sqlite.scm
endif

# pre-inst-env.in
# almost ready, but let's added later, it still needs 
# some final touches
EXTRA_DIST = \
	AUTHORS				\
	ChangeLog			\
	COPYING				\
	INSTALL				\
	LICENSE				\
	NEWS				\
	README				\
	env.in				\
	meta/guild.in			\
	meta/grip-1.0.pc.in		\
	$(wildcard am/*)		\
	$(wildcard m4/*)		\
	$(SOURCES)			\
	$(NOCOMP_SOURCES)

# bin_SCRIPTS = grip/...

CLEANFILES = \
	meta/grip-1.0.pc		\
	$(GOBJECTS)
# $(bin_SCRIPTS)

if INSTALL_GRIP_GNOME
pkgconfig_DATA +=\
	meta/grip-gnome-1.0.pc
EXTRA_DIST += \
	meta/grip-gnome-1.0.pc.in
CLEANFILES += \
	meta/grip-gnome-1.0.pc
endif

if INSTALL_GRIP_CLUTTER
pkgconfig_DATA +=\
	meta/grip-clutter-1.0.pc
EXTRA_DIST += \
	meta/grip-clutter-1.0.pc.in
CLEANFILES += \
	meta/grip-clutter-1.0.pc	\
	grip/clutter/examples/bouncer	\
	grip/clutter/examples/clock	\
	grip/clutter/examples/grid	\
	grip/clutter/examples/spline	\
	grip/clutter/examples/toolbar	\
	grip/clutter/examples/template
endif

if INSTALL_GRIP_SQLITE
pkgconfig_DATA +=\
	meta/grip-sqlite-1.0.pc
EXTRA_DIST += \
	meta/grip-sqlite-1.0.pc.in
CLEANFILES = \
	meta/grip-sqlite-1.0.pc
endif

# ETAGS_ARGS = \
# 	$(SOURCES)

SUBDIRS = \
	doc

# dist-hook: gen-ChangeLog

distclean-local:
	rm -f Makefile.in
	rm -f aclocal.m4
	rm -rf autom4te.cache
	rm -rf build-aux
	rm -f configure
	rm -f *~
	rm -f grip-*.tar.gz
	rm -rf doc/grip
	rm -f doc/*.info
	rm -f doc/*.pdf
	rm -f doc/*~
	rm -f doc/*.aux doc/*.cp doc/*.cps doc/*.fn doc/*.ky
	rm -f doc/*.log doc/*.pg doc/*.toc doc/*.tp doc/*.vr
	rm -f doc/Makefile.in
	rm -f doc/stamp-vti
	rm -f grip/*~
	rm -f grip/TAGS

# * .PHONY: gen-ChangeLog gen-pof:

# gen-ChangeLog:
# 	if test -d .git; then					\
# 	  rm -f $(distdir)/ChangeLog;				\
# 	  $(top_srcdir)/build-aux/gitlog-to-changelog > 	\
# 		$(distdir)/ChangeLog;				\
# 	  cp $(distdir)/ChangeLog $(top_srcdir)/ChangeLog;	\
# 	fi

# Makefile.am ends here
