#!/bin/sh

export DISPLAY=:1
#export LD_PRELOAD="/home/santos/.tgd/libs/libNvidia_gfx_debugger.so /usr/lib/nvidia-364-prime/libGL.so.1"
#export LD_LIBRARY_PATH="/usr/lib/nvidia-364/"
/home/santos/bin/lein trampoline run -m examples.palette



