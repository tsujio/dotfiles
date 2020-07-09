#!/bin/bash

script_dir=$(cd $(dirname $0); pwd)

[ -e ${HOME}/.emacs.d ] || ln -s $script_dir/.emacs.d ${HOME}/.emacs.d
