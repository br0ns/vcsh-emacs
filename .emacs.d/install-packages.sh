#!/bin/sh
cd "$(readlink -f $(dirname 0))"
$(which emacs) -Q --batch -l "install-packages.el"
rm -f "install-packages.el~"
rm -rf "auto-save-list"
mkdir -p \
    "savefiles/auto-save" \
    "savefiles/auto-backup" \
    "savefiles/auto-save-list" \
    "savefiles/undo-tree"
