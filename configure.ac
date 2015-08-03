
# check for one particular file of the sources 
AC_INIT([ocaml-lastfm],[0.3.1],[savonet-users@lists.sourceforge.net])

VERSION=$PACKAGE_VERSION
AC_SUBST(VERSION)
AC_MSG_RESULT([configuring $PACKAGE_STRING])

AC_BASE_CHECKS()

AC_MSG_CHECKING(for ocaml-pcre)
   if ! $OCAMLFIND query pcre > /dev/null 2>&1 ; then
       AC_MSG_ERROR(Not found.)
   else
   INC="$INC `$OCAMLFIND query pcre`"
   requires="$requires pcre"
   AC_MSG_RESULT(ok)
fi

#
# xmlplaylist
#

AC_ARG_WITH([xmlplaylist-dir],AS_HELP_STRING([--with-xmlplaylist-dir=path],[use "path" as the location of ocaml-xmlplaylist (autodetected by default)]))
if test -z "$with_xmlplaylist_dir"; then
  AC_MSG_CHECKING(for ocaml-xmlplaylist)
     if ! $OCAMLFIND query xmlplaylist > /dev/null 2>&1 ; then
         AC_MSG_ERROR(Not found.)
     fi
     INC="$INC `$OCAMLFIND query xmlplaylist`"
     requires="$requires xmlplaylist"
     AC_MSG_RESULT(ok)
else
  echo $with_xmlplaylist_dir | grep ^/ > /dev/null 2>&1 \
  || with_xmlplaylist_dir=$PWD/$with_xmlplaylist_dir
  _INC="$_INC $with_xmlplaylist_dir"
  _requires="$_requires xmlplaylist"
fi

#
# ocamlnet
#

AC_MSG_CHECKING(for netclient from ocamlnet)
   if ! $OCAMLFIND query netclient > /dev/null 2>&1 ; then
       AC_MSG_RESULT([Not found, will only build generic modules.])
       OCAMLNET_FILES=""
   else
       INC="$INC `$OCAMLFIND query netclient`"
       requires="$requires netclient"
       AC_MSG_RESULT(ok)
       OCAMLNET_FILES="lastfm.mli lastfm.ml"
fi

if test -n "$OCAMLNET_FILES" ; then
AC_MSG_CHECKING(for netstring from ocamlnet)
   if ! $OCAMLFIND query netstring > /dev/null 2>&1 ; then
       AC_MSG_ERROR(Not found.)
   else
       INC="$INC `$OCAMLFIND query netstring`"
       requires="$requires netstring"
       AC_MSG_RESULT(ok)
fi
fi

# Bytes
AC_CHECK_OCAML_BINDING([bytes],[],[],[1])
INC="$INC `$OCAMLFIND query bytes`"

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(OCAMLNET_FILES)
ALL_INC="`ocamlfind query -r -separator " " $requires` $_INC"
INC="$INC $_INC"
AC_SUBST(INC)
AC_SUBST(ALL_INC)
all_requires="`ocamlfind query -r -separator " " -format "%p" $requires` $_requires"
requires="$requires $_requires"
AC_SUBST(requires)
AC_SUBST(all_requires)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([src/lastfm_constants.ml])
AC_CONFIG_FILES([examples/Makefile.request])
AC_CONFIG_FILES([examples/Makefile.submit])
AC_CONFIG_FILES([examples/Makefile.download])
AC_OUTPUT
