#!/bin/bash

#   Copyright (c) 2025-2026 Jerome Shidel
#   The Clear BSD License
#   All rights reserved.

#   This unit is part of the MPLA frame available at:
#   https://gitlab.com/mpla-oss/mpla/

BUILD_DATE=$(date "+%Y-%m-%d %H:%M:%S")

function parse_attr() {
  local key="<${1}"
  line="$(cat $lpi | grep -m 1 ${key})"
  line=$(echo $line | cut -d ' ' -f 2-)
  [[ ${#line} -gt 2 ]] && line=${line:0:$((${#line} - 2))} || line='';
  while [[ $line != '' ]] ; do
  	lopt=${line%%'='*}
  	line=${line:$(( ${#lopt} + 2 ))}
  	lval=${line%%'"'*}
  	line=$(echo ${line:$(( ${#lval} + 1 ))})
  	topt="${2}_$(echo $lopt | tr [:lower:] [:upper:])"
  	tval="$lval"
  	[[ "${topt}" == "APP_/" ]] && continue
    # echo ${topt}=\"$tval\"
  	eval $topt=\"$tval\"
  	if [[ "$2" = "APP" ]] ; then
		opts[${#opts[@]}]="$topt"
		vals[${#vals[@]}]="$lval"
	fi;
  done;
}

function parse_value() {
  local key="<${1}"
  local k="${1}"
  line="$(cat $lpi | grep -m 1 ${key})"
  line=$(echo $line | cut -d ' ' -f 2-)
  [[ ${#line} -gt 2 ]] && line=${line:0:$((${#line} - 2))} || line='';

  lopt=${line%%'='*}
  line=${line:$(( ${#lopt} + 2 ))}
  lval=${line%%'"'*}
  [[ "${2}" != "" ]] && k="${2}_${k}"
  topt="$(echo ${k}_${lopt} | tr [:lower:] [:upper:])"
  tval="$lval"

  # echo ${topt}=\"$tval\"
  eval $topt=\"$tval\"

}

function TF () {
	if [[ "$1" = "True" ]] ; then
		echo True
	else
		echo False
	fi;
}

getval () {
	local val=$(cat $lpi | grep -m 1 "$1" | cut -d '"' -f 2)
	[[ $val ]] && echo $val || echo '0'
}

print_consts() {
	echo "{ Application Version Information File                                 }"
	echo "{                                                                      }"
	echo "{ This file is created automatically by the version.sh script whenever }"
	echo "{ the project is built by Lazarus. Manual changes will be lost.        }"
	echo

	echo "unit Version;"
	echo
	echo '{$mode ObjFPC}{$H+}'
	echo
	echo '{$I patches.pp}  // Various compiler directives to "fix" things.'
	echo
	echo 'interface'
	echo
	echo '{$IFDEF USES_CWString}'
	echo '  uses cwstring;'
	echo '{$ENDIF}'
	echo

	echo "const"
	echo "  { General Application Information }"
	[[ "${APP_IDENTIFIER}" != '' ]] && echo "  APP_IDENTIFIER: String = '${APP_IDENTIFIER}';"
	echo "  APP_VERSION: String = '${APP_VERSION}';"
	echo "  APP_BUILD: String = '${APP_BUILD}';"

	i=0;
	while [[ i -lt ${#opts[@]} ]] ; do
		if [[ "${opts[$i]}" != 'APP_/' ]] ; then
			echo "  ${opts[$i]}: String = '${vals[$i]}';"
		fi
		(( i++ ))
	done
	echo "  APP_YEAR: String = '${BUILD_YEAR}';"

	echo
    echo 'implementation'
	echo
	echo 'end.'

}

unordef () {

	if [[ "$2" != '' ]] ; then
		local flag="{\$DEFINE ${1}}${note}"
	else
		local flag="{\$UNDEF ${1}}${note}"
	fi

	if [[ "${3}" != '' ]] ; then
		while [[ ${#flag} -lt 32 ]] ; do
		  flag="${flag} "
		done
		flag="${flag} { ${3} }"
	fi

	echo "${flag}"

}

print_defs () {

	echo "{ Application Version Definitions File                                 }"
	echo "{                                                                      }"
	echo "{ This file is created automatically by the version.sh script whenever }"
	echo "{ the project is built by Lazarus. Manual changes will be lost.        }"
	echo

	echo
	echo '{ Version build flags defined }'
	echo

	unordef BUILD_DEBUG "$ATTR_PVADEBUG" "enable additional log messages"
	unordef BUILD_PRERELEASE "$ATTR_PVAPRERELEASE" "enable NLS message creation support"
	unordef BUILD_PATCHED "$ATTR_PVAPATCHED"
	unordef BUILD_PRIVATE "$ATTR_PVAPRIVATEBUILD"
	unordef BUILD_SPECIAL "$ATTR_PVASPECIALBUILD"

    echo
	unordef BUILD_LCLSCALED "${VAR_SCALED_VALUE}"
	unordef BUILD_XPMANIFEST "${VAR_USEXPMANIFEST_VALUE}"
	unordef BUILD_XPDPIAWARE "${VAR_DPIAWARE_VALUE}"

	echo
	echo '{ Version info strings defined }'
	echo

	unordef BUILD_INTERNALNAME "$APP_INTERNALNAME" "REQUIRED! MANDATORY!"

	unordef BUILD_PRODUCTNAME "$APP_PRODUCTNAME"
	unordef BUILD_TITLE "$APP_TITLE" "needed for application title"
	unordef BUILD_LEGALCOPYRIGHT "$APP_LEGALCOPYRIGHT" "needed for copyright info"
	unordef BUILD_FILEVERSION "$APP_VERSION" "required for update checking"
	unordef BUILD_ORIGINALFILENAME "$APP_ORIGINALFILENAME"
	unordef BUILD_FILEDESCRIPTION "$APP_FILEDESCRIPTION"
	unordef BUILD_COMMENTS "$APP_COMMENTS"
	unordef BUILD_LEGALTRADEMARKS "$APP_LEGALTRADEMARKS"
	unordef BUILD_PRODUCTVERSION "$APP_PRODUCTVERSION" "wanted for mac app bundle"
	unordef BUILD_COMPANYNAME "$APP_COMPANYNAME" "wanted for mac app bundle"

	echo

}

cwd="${PWD}"

version_file='version.inc'

[[ $1 ]] && {
  [[ -f "${1}.lpi" ]] && lpi="${1}.lpi"
  [[ "${1:(-3)}" = 'lpi' ]] && [[ -f "${1}" ]] && lpi="${1}"
  [[ $lpi ]] && version_file="${lpi:0:(( ${#lpi} - 4 ))}.inc"
}

[[ $lpi = '' ]] && lpi=$(ls *.lpi 2>&1 | grep -v '*' | grep -m 1 '.lpi');

if [[ $lpi = '' ]] ; then
  echo Lazarus Project Information file not found. >&2
else
  parse_value Title VAR
  # Set APP_TITLE
  APP_TITLE="${VAR_TITLE_VALUE}"
  opts[${#opts[@]}]="APP_TITLE"
  vals[${#vals[@]}]="${APP_TITLE}"

  parse_value Scaled VAR
  parse_value UseXPManifest VAR
  parse_value DpiAware VAR

  parse_attr StringTable APP
  parse_attr Attributes ATTR
  parse_attr Language LANG
  parse_attr CharSet  CHRS

  # APP_VENDOR used in generating the APP_IDENTIFIER
  APP_VENDOR="${APP_COMPANYNAME}"

fi;

APP_VERSION=$(getval MajorVersion)'.'$(getval MinorVersionNr)'.'$(getval RevisionNr)
APP_BUILD=$(getval BuildNr)

# Always Present Constants
[[ ! $APP_TITLE ]] && APP_TITLE='Unknown';
[[ ! $APP_VENDOR ]] && APP_VENDOR='Company';
if [[ ${APP_INTERNALNAME} ]] ; then
	[[ ! $APP_IDENTIFIER ]] && APP_IDENTIFIER=$(echo 'com.'${APP_VENDOR}'.'${APP_INTERNALNAME} | tr [:upper:] [:lower:] | tr -d ' ')
fi
[[ ! $APP_VERSION ]] && APP_VERSION='0.0.0';
[[ ! $APP_BUILD ]] && APP_VERSION='';


if [[ ! $APP_LEGALCOPYRIGHT ]] ; then
	if [[ "$APP_COMPANYNAME" != "" ]] ; then
		APP_LEGALCOPYRIGHT="(c) ${BUILD_YEAR}, $APP_COMPANYNAME"
		opts[${#opts[@]}]="APP_LEGALCOPYRIGHT"
		vals[${#vals[@]}]="(c) ${BUILD_YEAR}, $APP_COMPANYNAME"
	fi
fi

print_consts >$version_file
mv $version_file version.pp
print_defs >$version_file
mv $version_file version.def

echo "${APP_PRODUCTNAME}" >&2
echo "${APP_TITLE}, version ${APP_VERSION}" >&2
echo "${APP_LEGALCOPYRIGHT}" >&2

exit 0
