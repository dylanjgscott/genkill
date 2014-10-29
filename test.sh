#!/bin/sh

OPTIND=1

DEAD=
UNRE=
LOAD=
OUTFILE=
INFILE=

while getopts "udlo:" opt; do
    case "$opt" in
    \?)
        echo "unrecognised symbol"
        exit 0
        ;;
    u)
        UNRE="-u"
        ;;
    d)
        DEAD="-d"
        ;;
    l)
        LOAD="-l"
        ;;
    o)
        FILE=$OPTARG
    esac
done

shift $((OPTIND -1))

INFILE=$@

if [ -z "$OUTFILE" ]; then
    ./optimizer $UNRE $DEAD $LOAD $INFILE
else
    ./optimizer $UNRE $DEAD $LOAD $INFILE > $OUTFILE
fi
