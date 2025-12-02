 #!/bin/sh

bnfc -m basic.cf
make
make clean

./TestBasic < test/hallo_welt.basic
./TestBasic < test/test_prog.basic

make distclean