#!/bin/sh

OPTIND=1

OPTIMIZER=./optimizer

DEAD=
LOAD=
UNREACHABLE=
OUTFILE=
INFILE=

usage () {
echo -e "Usage: optimize.sh [OPTION]... FILE"
echo -e "\t-h\tThis help message."
echo -e "\t-u\tTurn on unreachable code elimination"
echo -e "\t-d\tTurn on dead code elimination."
echo -e "\t-l\tTurn on load optimisations."
echo -e "\t-o FILE\tWrite the output to FILE rather than stdout."
}

while getopts hudlo: opt; do
	case "$opt" in
	\?)
		usage
		exit -1
		;;
	h)
		usage
		exit 0
		;;
	u)
		UNREACHABLE="-u"
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

if [ ! -f $OPTIMIZER ]; then
	echo -e "optimizer does not exist"
	usage
	exit -1
fi

if [ ! -x $OPTIMIZER ]; then
	echo -e "optimizer not executable"
	usage
	exit -1
fi

if [ -z "$INFILE" ]; then
	echo -e "no file provided"
	usage
	exit -1
fi

if [ ! -f $INFILE ]; then
	echo -e "file does not exist"
	usage
	exit -1
fi

if [ ! -r $INFILE ]; then
	echo -e "unable to read file"
	usage
	exit -1
fi

if [ -z "$OUTFILE" ]; then
	./optimizer $UNREACHABLE $DEAD $LOAD $INFILE
else
	./optimizer $UNREACHABLE $DEAD $LOAD $INFILE > $OUTFILE
fi
