#!/bin/sh
#
# ~/.xinitrc
#
xset r rate 180 40                   # Sane repeat rate
xset -b                              # No bell
# xset -dpms                           # Keep screen on at all times
xset s off                           #
xset m 4/3 0                         # Pointer settings
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

xautolock -time 5 -locker slock &

~/.fehbg &

exec xmonad
