#!/bin/sh
#
# ~/.xinitrc
#
xset r rate 160 80                   # Sane repeat rate
xset -b                              # No bell
xset dpms 180 300 600
xset s 120 0                         #
xset m 7/5 0												 # Pointer settings
setxkbmap -option compose:super      # Set compose key to R-Alt

if [ -f ~/.nvidia-settings-rc ]; then
  nvidia-settings --load-config-only;
fi

if [ -f ~/.xbindkeysrc ]; then
  xbindkeys;
fi

if [ -f ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap;
fi

if [ -f ~/.Xresources ]; then
  xrdb -merge ~/.Xresources
fi

xss-lock -- /usr/bin/slock &

exec xmonad
