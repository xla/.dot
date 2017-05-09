#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

[ -z "$DISPLAY" -a "$(fgconsole)" -eq 1 ] && exec startx
