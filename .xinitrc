#!/bin/sh
xmodmap ~/.Xmodmap
dunst &
redshift &
# Script for setting up my keyboard(for thai keyboard and caps to control)
~/.local/bin/keyboard
~/.fehbg &
exec $HOME/.cabal/bin/xmonad
