#! /bin/bash
ROOTDIR=`cd $(dirname $0); pwd`
NODE_NAME="extend104"
CONFIG_PATH="$ROOTDIR/etc/${NODE_NAME}"

export ERL_LIBS=lib
erl -sname extend104 -pa ebin -config $CONFIG_PATH \
	-boot start_sasl \
    -s extend104_app start -s reloader start

