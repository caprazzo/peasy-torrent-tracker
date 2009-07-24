#!/bin/sh

erl -boot start_sasl -config config/cfg_qa -detached -name peasy -cookie peasy -pa ./ebin -pa ./ebin/mochiweb -s peasy start