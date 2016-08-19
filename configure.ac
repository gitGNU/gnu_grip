dnl -*- Autoconf -*-
dnl
dnl   configuration script for Grip
dnl   Process this file with autoconf to produce configure.
dnl

define(GRIP_CONFIGURE_COPYRIGHT,[[

Copyright (C) 2015 - 2016
David Pirotte <david at altosw dot be>

This file is part of Grip.

Grip is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Grip is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Grip.  If not, see <http://www.gnu.org/licenses/>.

]])

AC_PREREQ(2.69)

AC_INIT([grip], [0.1.1], [david at altosw dot be])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
dnl Use `serial-tests' so the output `check-guile' is not hidden
dnl (`parallel-tests' is the default in Automake 1.13.)
dnl `serial-tests' was introduced in Automake 1.12.
AM_INIT_AUTOMAKE([1.14 gnu -Wall -Werror -Wno-portability -Wno-syntax \
  color-tests serial-tests foreign])
AM_MAINTAINER_MODE([enable])
AM_SILENT_RULES([yes])

AC_COPYRIGHT(GRIP_CONFIGURE_COPYRIGHT)
AC_CONFIG_SRCDIR([grip])


PACKAGES_TO_INSTALL=""
PACKAGES_NOT_INSTALLED=""

record_check()
{
    local package=$1;
    local buildp=$2;
    case $buildp in
        yes|true)
            if test -z "$PACKAGES_TO_INSTALL"; then
                PACKAGES_TO_INSTALL="$package"
            else
                PACKAGES_TO_INSTALL="$PACKAGES_TO_INSTALL $package"
            fi
            ;;
        no|false)
            if test -z "$PACKAGES_NOT_INSTALLED"; then
                PACKAGES_NOT_INSTALLED="$package"
            else
                PACKAGES_NOT_INSTALLED="$PACKAGES_NOT_INSTALLED $package"
            fi
            ;;
        *)
            echo "bad record_check value: \"$buildp\" (for package \"$package\")"
            exit 1
            ;;
    esac
}


AC_CHECK_PROG(have_makeinfo, makeinfo, yes, no)
AM_CONDITIONAL(HAVE_MAKEINFO, test "$have_makeinfo" = yes)

GUILE_PKG([2.2 2.0])
GUILE_PROGS([2.0.11])
GUILE_PREFIX_DIR
GUILE_SITE_DIR
GUILE_GLOBAL_SITE_DIR
GUILE_SITE_CCACHE_DIR

GUILE_GNOME_PROGS_AVAILABLE([HAVE_GUILE_GNOME], [2.16.4])
GUILE_MODULE_AVAILABLE([HAVE_GNOME_2], [(gnome-2)])
GUILE_GNOME_INST_DIR
GUILE_GNOME_LIBS_PATH
GUILE_GNOME_FLAGS
AC_SUBST(HAVE_GNOME_2)
dnl guile-gnome is a set of wrappers, we need to check that the user
dnl has wrapped the gnome libraries grip needs.  strictly speaking,
dnl the list below is 'too much' because some guile-gnome modules are
dnl always part of guile-gnome, but hey, no arm and nice to see all
dnl the checks.
GUILE_GNOME_MODULE_AVAILABLE(
  [HAVE_GUILE_GNOME_MODULES],
  [(gnome glib) \
(gnome gobject) \
(gnome gtk) \
(gnome gtk gdk-event) \
(gnome gnome) \
(gnome gnome-ui) \
(gnome glade)])

AS_IF([test "x$HAVE_GUILE_GNOME" = xyes \
         -a "x$HAVE_GNOME_2" = xyes \
         -a "x$HAVE_GUILE_GNOME_MODULES" = xyes],
      [install_grip_gnome=yes],
      [install_grip_gnome=no])

AM_CONDITIONAL([INSTALL_GRIP_GNOME],
               [test $install_grip_gnome = yes])
record_check guile-gnome $install_grip_gnome

PKG_CHECK_MODULES(
  [GUILE_CLUTTER],
  [guile-clutter-1.0 >= 1.12.2.1],
  [HAVE_GUILE_CLUTTER=yes],
  [HAVE_GUILE_CLUTTER=no])

AS_IF([test "x$HAVE_GUILE_CLUTTER" = xyes],
      [install_grip_clutter=yes],
      [install_grip_clutter=no])

AM_CONDITIONAL([INSTALL_GRIP_CLUTTER],
               [test $install_grip_clutter = yes])
record_check guile-clutter $install_grip_clutter


# PKG_CHECK_MODULES(libpcre, libpcre)

PKG_CHECK_MODULES(
  [SQLite3],
  [sqlite3 >= 3.7],
  [HAVE_SQLITE=yes],
  [HAVE_SQLITE=no])

AS_IF([test "x$HAVE_SQLITE" = xyes],
      [install_grip_sqlite=yes],
      [install_grip_sqlite=no])

AM_CONDITIONAL([INSTALL_GRIP_SQLITE],
               [test $install_grip_sqlite = yes])
record_check sqlite3 $install_grip_sqlite


# AC_CHECK_PROG(PDFLATEX, pdflatex, yes)
# if test -z "$PDFLATEX"; then
#   AC_MSG_ERROR([pdflatex required but not found])
# fi

AC_CONFIG_FILES(
  [env],
  [chmod +x env])
# AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES(
  [meta/guild],
  [chmod +x meta/guild])
AC_CONFIG_FILES(
  [grip/clutter/examples/template],
  [chmod +x grip/clutter/examples/template])
AC_CONFIG_FILES(
  [grip/clutter/examples/bouncer],
  [chmod +x grip/clutter/examples/bouncer])
AC_CONFIG_FILES(
  [grip/clutter/examples/clock],
  [chmod +x grip/clutter/examples/clock])
AC_CONFIG_FILES(
  [grip/clutter/examples/grid],
  [chmod +x grip/clutter/examples/grid])
#AC_CONFIG_FILES(
#  [grip/clutter/examples/hello],
#  [chmod +x grip/clutter/examples/hello])
AC_CONFIG_FILES(
  [grip/clutter/examples/spline],
  [chmod +x grip/clutter/examples/spline])
AC_CONFIG_FILES(
  [grip/clutter/examples/toolbar],
  [chmod +x grip/clutter/examples/toolbar])
AC_CONFIG_FILES(
  [grip/clutter/examples/scroll],
  [chmod +x grip/clutter/examples/scroll])
AC_CONFIG_FILES(
  [grip/clutter/examples/drag],
  [chmod +x grip/clutter/examples/drag])
AC_CONFIG_FILES(
  [grip/clutter/examples/drop],
  [chmod +x grip/clutter/examples/drop])
AC_CONFIG_FILES([
	meta/grip-1.0.pc
	meta/grip-clutter-1.0.pc
	meta/grip-gnome-1.0.pc
	meta/grip-sqlite-1.0.pc
	Makefile
	doc/Makefile])
AC_OUTPUT
