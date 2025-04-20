#!/bin/sh

# this bash script will retrieve the version/revision of FPC, Lazarus and the current
# programs source tree and store them in the $version_file pascal file.

BUILD_DATE=$(date "+%Y-%m-%d %H:%M:%S")

function parse_attr() {
  line="$(cat $lpi | grep -m 1 ${1})"
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
  	eval $topt=\"$tval\"
  	if [[ "$2" = "APP" ]] ; then
		opts[${#opts[@]}]="$topt"
		vals[${#vals[@]}]="$lval"
	fi;
  done;
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
	echo "// Application Version Information File"
	echo
	echo "// This file is created automatically by the version.sh script whenever"
	echo "// the project is built by Lazarus. Manual changes will be lost."
	echo

	echo "const"
	# Type Declaration
	echo "  { The default Free Pascal Compiler }"
	echo "  FPC_VERSION='$FPC_VERSION';"
	echo "  FPC_PLATFORM='$FPC_PLATFORM';"
	echo "  FPC_TARGET='$FPC_TARGET';"
	echo
	if [[ "${LAZARUS_VERSION}" != "" ]] ; then
		echo "  { The Lazarus I.D.E }"
		echo "  LAZARUS_VERSION='$LAZARUS_VERSION';"
	fi
	echo
	echo "  { Source & Subversion Last Changed Commit }"
	echo "  SOURCE_VERSION='${APP_VERSION}';"
	echo "  SOURCE_REVISION='$REVISION';"
	echo "  SOURCE_URL='$URL';"
    echo "  SOURCE_COMMIT='$REVISION_ID';"
	echo
    echo "  { Version Build Atributes } "
	echo "  BUILD_DEBUG="$(TF $ATTR_PVADEBUG)";"
	echo "  BUILD_PRERELEASE="$(TF $ATTR_PVAPRERELEASE)";"
	echo "  BUILD_PATCHED="$(TF $ATTR_PVAPATCHED)";"
	echo "  BUILD_PRIVATE="$(TF $ATTR_PVAPRIVATEBUILD)";"
	echo "  BUILD_SPECIAL="$(TF $ATTR_PVASPECIALBUILD)";"
	[[ "$LANG_VALUE" != "" ]] && echo "  BUILD_LANGUAGE='$LANG_VALUE';"
	[[ "$CHRS_VALUE" != "" ]] && echo "  BUILD_CHARSET='$CHRS_VALUE';"
	echo "  BUILD_DATE='$BUILD_DATE';"
	echo
	echo "  { General Application Information }"
	echo "  APP_IDENTIFIER='${APP_IDENTIFIER}';"
	echo "  APP_VERSION='${APP_VERSION}';"
	echo "  APP_BUILD='${APP_BUILD}';"

	i=0;
	while [[ i -lt ${#opts[@]} ]] ; do
		if [[ "${opts[$i]}" != 'APP_/' ]] ; then
			echo "  ${opts[$i]}='${vals[$i]}';"
		fi
		(( i++ ))
	done
	echo "  APP_YEAR='${BUILD_YEAR}';"

	echo
    # echo '{$ENDIF}'

}

stats() {
	FPC_REVISION=$(echo $FPC_REVISION | cut -d "'" -f 2 )
	LAZARUS_VERSION=$(echo $LAZARUS_VERSION | cut -d "'" -f 2)
	echo "$FPC_TARGET FPC Version $FPC_VERSION"
	echo "Lazarus Version $LAZARUS_VERSION"

	/bin/echo -n "$APP_TITLE Version $APP_VERSION"
	[[ $APP_BUILD ]] && /bin/echo -n ", build $APP_BUILD"
	[[ $REVISION ]] && /bin/echo -n " (r$REVISION)"
	echo
	return 0
}

function cvs_git () {
    echo "Retrieve project git data for ${PWD}."
    REVISION=$(wc -l .git/logs/head 2>/dev/null | cut -d '.' -f 1 )
    [[ $? = 0 ]] && (( REVISION++ )) || REVISION=0
    REV_HEAD=$(cat .git/HEAD 2>/dev/null | cut -d ' ' -f 2- )
    REVISION_ID=$(cat .git/${REV_HEAD} 2>/dev/null)
    ONLINE_ID=$(cat .git/refs/remotes/origin/${REV_HEAD##*/} 2>/dev/null)
    URL=$(grep -i "URL=\|URL =" .git/config | cut -d '@' -f 2-)
    URL="${URL/://}"
    URL="http://${URL%.*}"
}

BUILD_YEAR="${BUILD_DATE%%-*}"

cwd="${PWD}"

while [[ "$PWD" != '/' ]] && [[ ! -d '.git' ]] ; do
    cd ..
done
[[ -d '.git' ]] && cvs_git ${vopts}
cd "$cwd"

FPC_VERSION=$(/usr/local/bin/fpc -iV)
FPC_TARGET=$(/usr/local/bin/fpc -iTP)
FPC_PLATFORM=$(/usr/local/bin/fpc -iTO)
LAZARUS_VERSION=$(/usr/local/bin/lazbuild -v)

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
  parse_attr StringTable APP
  parse_attr Attributes ATTR
  parse_attr Language LANG
  parse_attr CharSet  CHRS

  APP_VENDOR=$APP_COMPANYNAME
  APP_TITLE=$APP_PRODUCTNAME
fi;

APP_VERSION=$(getval MajorVersion)'.'$(getval MinorVersionNr)'.'$(getval RevisionNr)
APP_BUILD=$(getval BuildNr)

# Always Present Constants
[[ ! $FPC_VERSION ]] && FPC_REVISION="unknown"
[[ ! $FPC_PLATFORM ]] && FPC_REVISION="unknown"
[[ ! $FPC_TARGET ]] && FPC_TARGET="unknown"
[[ ! $LAZARUS_VERSION ]] && LAZARUS_VERSION="unknown"
[[ ! $REVISION ]] && REVISION=""
[[ ! $URL ]] && URL="";
[[ ! $APP_TITLE ]] && APP_TITLE='Unknown';
[[ ! $APP_VENDOR ]] && APP_VENDOR='Company';
[[ ! $APP_IDENTIFIER ]] && APP_IDENTIFIER=$(echo 'com.'${APP_VENDOR}'.'${APP_INTERNALNAME} | tr [:upper:] [:lower:] | tr -d ' ')
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
mv $version_file ${0%.*}.inc

stats >&2
exit 0
