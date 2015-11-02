#! /bin/sh
BASEDIR=`dirname $0`
BASEDIR=`readlink -f $BASEDIR`
cd $BASEDIR

ERL_LIBS=$BASEDIR:$BASEDIR/deps
export ERL_LIBS

SNAME=adventures@localhost


if [ "$1" = "shell" ]; then
    erl -remsh $SNAME -sname rem
else
    exec erl +K true -noinput -noshell \
        -sasl errlog_type error \
        -sname $SNAME \
        -s adventures_app
fi
