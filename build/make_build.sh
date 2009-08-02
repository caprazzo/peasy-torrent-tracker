#!/bin/sh

ERLDIR=$(./make_build.escript)
rm -rf FIRST
#erts-5.7.2/ lib/ releases/ bin/
mkdir FIRST
cd FIRST; tar xvzf ../FIRST.tar.gz
mkdir -p bin log
sed -e "s|^ROOTDIR=.*$|ROOTDIR=$(PWD)|" < erts-5.7.2/bin/start > bin/start
chmod +x bin/start
cp erts-5.7.2/bin/run_erl bin/
cp erts-5.7.2/bin/start_erl.src bin/start_erl
cp erts-5.7.2/bin/to_erl bin/
chmod +x bin/start_erl
cp $ERLDIR/bin/start_sasl.boot bin/start.boot

sed -e "s|^ROOTDIR=.*|ROOTDIR=$(PWD)|" < erts-5.7.2/bin/erl > bin/erl
echo "5.7.2 1.0" > releases/start_erl.data

../create_releases.escript $(PWD)
