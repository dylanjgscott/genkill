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
echo -e "\nIf no options are given, all optimisations will be used."
}

if [ $# -eq 0 ]; then
    usage
    exit
fi

while getopts hudlo: opt; do
	case $opt in
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
        OUTFILE=$OPTARG
	esac
done

shift $((OPTIND -1))

INFILE=$@

if [ ! -f $OPTIMIZER ] || [ ! -x $OPTIMIZER ]; then
	echo -e "could not run the optimizer"
	usage
	exit -1
fi


if [ -z $INFILE ]; then
	echo -e "no file specified"
	usage
	exit -1
fi

if [ ! -f $INFILE ] || [ ! -r $INFILE ]; then
	echo -e "unable to open file"
	usage
	exit -1
fi

if [ -z $UNREACHABLE ] && [ -z $DEAD ] && [ -z $LOAD ]; then
	DEAD="-d"
	LOAD="-l"
	UNREACHABLE="-u"
fi

if [ -z $OUTFILE ]; then
	./optimizer $UNREACHABLE $DEAD $LOAD $INFILE
else
	./optimizer $UNREACHABLE $DEAD $LOAD $INFILE > $OUTFILE
fi
