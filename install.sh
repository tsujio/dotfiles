#!/bin/bash

script_dir=$(cd $(dirname $0); pwd)

[ -e ${HOME}/.zshrc ] || ln -s $script_dir/.zshrc ${HOME}/.zshrc
[ -e ${HOME}/.gitconfig ] || ln -s $script_dir/.gitconfig.linux ${HOME}/.gitconfig
[ -e ${HOME}/.emacs.d ] || ln -s $script_dir/.emacs.d ${HOME}/.emacs.d
