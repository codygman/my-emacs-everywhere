#!/usr/bin/env bash
set -u

export CUSTOMHOME="/tmp/my-emacs-everywhere"
export EMACSD="$CUSTOMHOME/.emacs.d"

echo "restore straight cache if exists"
cp -R $EMACSD/straight $EMACSD/straight  2> /dev/null

echo "removing EMACSD: $EMACSD"
rm -r $EMACSD 2> /dev/null
echo "DONE removing EMACSD: $EMACSD"

mkdir -p $CUSTOMHOME
cp -R ~/my-emacs-everywhere/ $EMACSD
cd $EMACSD
# tree
emacs -nw --load load-init-then-run-ert.el

echo "save straight cache"
cp -R $EMACSD/straight "$CUSTOMHOME"

echo "removing EMACSD: $EMACSD"
rm -rf $EMACSD 2> /dev/null
echo "DONE removing EMACSD: $EMACSD"
echo "left /tmp/my-emacs-everywhere to preserve straight install cache for next local run"
