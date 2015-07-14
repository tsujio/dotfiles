#!/bin/bash

DOT_FILES=( .zshrc .gitconfig.linux .emacs.d )

# Create symlinks into home directory.
for file in ${DOT_FILES[@]}; do
    if [ -e ${HOME}/${file} ]; then
	echo "${HOME}/${file} exists." 1>&2
	continue
    fi

    ln -s ${HOME}/dotfiles/${file} ${HOME}/${file}
    if [ $? -eq 0 ]; then
	echo "Created link ${file}."
    fi
done
