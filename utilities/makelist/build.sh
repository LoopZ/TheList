#!/bin/bash

# I don't want the command line build to increment the build number automatically
# Backup the LPI file. After the build, restore those files to reset the build number.

function build_default () {

	local app="${1}"
	local cpu=$(uname -m)
	local os=$(uname -s)
	[[ "${os}" == "Darwin" ]] && os=macOS
	cp ${app}.ver ${app}.lpi
	[[ -e ${app} ]] && rm ${app}
	lazbuild -B -r ${app}.lpr
#	if [[ -e ${app} ]] ; then
#		mkdir -p ../binaries/${cpu}/${os}
#		cp ${app} ../binaries/${cpu}/${os}/
#	fi

}

function build_app () {

	local app="${1}"

	cp ${app}.lpi ${app}.ver
	build_default ${app}
	cp ${app}.ver ${app}.lpi
	rm ${app}.ver

}

# [[ -d ../binaries ]] && rm -rf ../binaries

build_app fromrbil
build_app tolist


