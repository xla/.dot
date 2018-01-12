#
# ~/.bash_profile
#

export PATH=~/.local/bin:$PATH

[[ -f ~/.bashrc ]] && . ~/.bashrc

[ -z "$DISPLAY" -a "$(fgconsole)" -eq 1 ] && exec startx
