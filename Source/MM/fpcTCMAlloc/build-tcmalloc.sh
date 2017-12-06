#!/bin/sh

git submodule init
git submodule update

cd gperftools; \
sh autogen.sh; \
sh configure; \
make; \
make install; \
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/; \
cd ..

