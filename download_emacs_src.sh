#!/bin/bash

if [ ! -d "emacs-25.3" ]; then
    set -e
    set -x

    wget http://ftpmirror.gnu.org/emacs/emacs-25.3.tar.gz
    tar -xzf emacs-25.3.tar.gz
fi
