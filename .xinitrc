#!/bin/sh
#
# ~/.xinitrc
#

export PATH="/home/xla/.local/bin:$PATH"

xset r rate 160 80                   # Sane repeat rate
xset -b                              # No bell
# order: STANDBY SUSPEND OFF
xset dpms 300 600 720                # Energy mode switch in sec
xset dpms force off
xset s 300 0                         # Sets the timeout for the screen saver in sec
xset m 7/5 0                         # Pointer settings
setxkbmap -option compose:super      # Set compose key to R-Alt

if [ -f ~/.nvidia-settings-rc ]; then
  nvidia-settings --load-config-only;
fi

# Set us layout with accentuated keys.
setxkbmap us -variant altgr-intl

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

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

exec spectrwm
