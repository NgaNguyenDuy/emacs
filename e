#!/bin/bash

if [[ -z "$EMACS_INIT" ]]; then
    EMACS_INIT="$HOME/Contribute/emacs/src/init.el"
fi

emacs -Q -l $EMACS_INIT $* >/dev/null 2>&1 &disown 2>/dev/null
