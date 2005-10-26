#!/bin/sh

# LogicalShift site builder
# Written by Andrew Hunter

# Variables
STYLESHEET="./xsl/tamescheme.xsl"
CLASSPATH="./xsl"
SAXON="./xsl/saxon.jar"
XMLDIR="./xml"
HTMLDIR="html" # NOTE: will be destroyed if exists
SCHEMEDIR="tamescheme-snapshot"

# Sanity checks
if [ ! -d $XMLDIR ]; then
    echo "The '$XMLDIR' directory does not exist. Aborting"
    exit 1
fi

#if [ ! -f $SAXON ]; then
#    echo "The SAXON XSLT processor was not found. Aborting"
#    exit 1
#fi

if [ ! -f $STYLESHEET ]; then
    echo "The XSL stylesheet was not found. Aborting"
    exit 1
fi

if [ -e $HTMLDIR ]; then
    if [ ! -d $HTMLDIR ]; then
		echo "$HTMLDIR exists, but is not a directory"
		exit 1
    fi
fi

if [ -e $SCHEMEDIR ]; then
    if [ ! -d $SCHEMEDIR ]; then
		echo "$SCHEMEDIR exists, but is not a directory"
		exit 1
    fi
fi

# OKGO

echo "LogicalShift site generator version 0.1"
echo "Written by Andrew Hunter"
echo

# Set things up ready for processing
if [ -d $HTMLDIR ]; then
    echo -n "*** DELETING $HTMLDIR "
    rm -r $HTMLDIR
    echo "- Gone."
fi

echo -n "*** Initialising $HTMLDIR "
mkdir $HTMLDIR || exit 1
cp -R $XMLDIR/* $HTMLDIR/
echo "- Done."

# Used to do things this way - it's slow, so I wrote a utility to do this
# the fast way...

#echo -n "*** Processing XML with stylesheet >"
#IFS=$'\n'
#
#files=`find ${HTMLDIR} -name "*.xml"`
#
#for file in ${files}; do echo -n "."; done
#echo -n "<"$'\b'
#for file in ${files}; do echo -n $'\b'; done
#
#for file in ${files}; do
#    newfile=`sed 's/\.xml$/.html/' <<<${file}`
#    echo -n x$'\b'
#    java -jar "${SAXON}" "${file}" "${STYLESHEET}" >"${newfile}" || exit 1
#    echo -n X
#    rm "${file}"
#done
#echo "< - Done."

java -classpath $CLASSPATH saxonbatch $STYLESHEET $HTMLDIR

echo -n "*** Erasing backup files "
find ${HTMLDIR} -name "*~" -exec rm "{}" ";"
echo "- Done."

echo
echo OK.
echo

