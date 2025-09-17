#!/usr/bin/env bash
cabal install --overwrite-policy=always
command -v jj; [[ $? -eq 1 ]] && ln -s ~/.local/bin/juo ~/.local/bin/jj
