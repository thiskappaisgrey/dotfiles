#!/bin/sh
clight &
xmodmap ~/.Xmodmap
dunst &
# Script for setting up my keyboard(for thai keyboard and caps to control)
~/.local/bin/keyboard
~/.fehbg &
exec $HOME/.cabal/bin/xmonad
