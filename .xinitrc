#!/bin/sh
clight &
xmodmap ~/.Xmodmap
dunst &
# In case setxkbmap is not persistent I can just do it everytime I log in.
setxkbmap us,th
setxkbmap -option "grp:ctrls_toggle"
setxkbmap -option "ctrl:nocaps"
exec /usr/bin/xmonad
