#!/bin/bash

if [ ! -n "$1" ]; then
    echo "USAGE: ./iterm-ec.sh <filename> [lineno]";
else
    if [ ! -n "$2" ]; then
        ( /usr/local/bin/emacsclient "$1" & );
    else            
        ( /usr/local/bin/emacsclient "+$2" "$1" & );
    fi
fi

