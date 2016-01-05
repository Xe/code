#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([tirek], [0.0.1], [xena@yolo-swag.com])
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADERS([include/config.h])
AC_PREFIX_DEFAULT($HOME/prefix/tirek)
AC_LANG(C)

AC_CONFIG_MACRO_DIR([libratbox/m4])
AC_CONFIG_AUX_DIR([libratbox/tools])

AM_INIT_AUTOMAKE

AC_CONFIG_SUBDIRS(libratbox)

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

PKG_CHECK_MODULES([PY], [python3 >= 3.2])

AC_CONFIG_FILES(    \
    Makefile        \
    src/Makefile    \
)

AC_OUTPUT
