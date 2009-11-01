#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin \
    -boot start_sasl \
    -sname green \
    -setcookie VBry7ircDWxszBfA \
    -kernel inet_dist_listen_min 50000 inet_dist_listen_max 50100 \
    -s lightwave 
