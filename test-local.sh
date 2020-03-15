#!/usr/bin/env bash
set -u

export CUSTOMHOME="/tmp/my-emacs-everywhere"
export EMACSD="$CUSTOMHOME/.emacs.d"

echo "restore straight cache if exists"
mkdir -p $CUSTOMHOME
ls -alrth $CUSTOMHOME/straight
cp -R $CUSTOMHOME/straight $EMACSD/straight  2> /dev/null

echo "removing EMACSD: $EMACSD"
rm -rf $EMACSD 2> /dev/null
echo "DONE removing EMACSD: $EMACSD"

cp -r ~/my-emacs-everywhere/ $EMACSD
cd $EMACSD
# tree
export EMACSFOR="PERSONAL"
HOME=$CUSTOMHOME emacs -nw --load load-init-then-run-ert.el
# cat test-results.txt

# echo "save straight cache"
# mkdir -p straight
# echo "$EMACSD/straight"
# echo  "$CUSTOMHOME/straight"
# cp -R "$EMACSD/straight" "$CUSTOMHOME/straight"
# ls -larth

# echo "removing EMACSD: $EMACSD"
# rm -rf $EMACSD 2> /dev/null
# echo "DONE removing EMACSD: $EMACSD"
# echo "left /tmp/my-emacs-everywhere to preserve straight install cache for next local run"
