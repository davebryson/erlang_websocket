#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin ../../deps/mochiweb/ebin  ../../ebin -boot start_sasl -s basic
