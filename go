#! /bin/bash

make
export ERL_LIBS=lib
erl -pa ebin 

