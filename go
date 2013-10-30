#! /bin/bash
ROOTDIR=`cd $(dirname $0); pwd`
NODE_NAME="extend104"
CONFIG_PATH="$ROOTDIR/etc/${NODE_NAME}"
SASL_LOG_PATH="$ROOTDIR/log/${NODE_NAME}_sasl.log"
echo $CONFIG_PATH 

export ERL_LIBS=lib
erl -sname extend104 -pa ebin -config $CONFIG_PATH \
	-sasl sasl_error_logger \{file,\"$SASL_LOG_PATH\"\} -boot start_sasl \
    -s extend104_app start -s reloader start

