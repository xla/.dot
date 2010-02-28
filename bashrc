# MacPorts Installer addition on 2008-12-26_at_16:46:57: adding an appropriate PATH variable for use with MacPorts.
export PATH=/Users/alx/development/bin:/Users/alx/development/javascript/externals/narwhal/bin:/Users/alx/development/php/externals/cakephp/cake/console:/Users/alx/development/php/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

# MacPorts Installer addition on 2008-12-26_at_16:46:57: adding an appropriate MANPATH variable for use with MacPorts.
export MANPATH=/opt/local/share/man:$MANPATH
# Finished adapting your MANPATH environment variable for use with MacPorts.

export VIM_APP_DIR="/Applications/MacPorts/"

export PYTHONPATH="/Users/alx/development/python/site-packages":$PYTHONPATH

export ERL_LIBS=/Users/alx/development/erlang/externals/
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

alias ll='ls -lah'
alias nr='rlwrap node-repl'

# ssh-add .ssh/sc-keypair

# Colors from http://wiki.archlinux.org/index.php/Color_Bash_Prompt
# misc
NO_COLOR='\e[0m' #disable any colors
# regular colors
BLACK='\e[0;30m'
RED='\e[0;31m'
GREEN='\e[0;32m'
YELLOW='\e[0;33m'
BLUE='\e[0;34m'
MAGENTA='\e[0;35m'
CYAN='\e[0;36m'
WHITE='\e[0;37m'
# emphasized (bolded) colors
EBLACK='\e[1;30m'
ERED='\e[1;31m'
EGREEN='\e[1;32m'
EYELLOW='\e[1;33m'
EBLUE='\e[1;34m'
EMAGENTA='\e[1;35m'
ECYAN='\e[1;36m'
EWHITE='\e[1;37m'
# underlined colors
UBLACK='\e[4;30m'
URED='\e[4;31m'
UGREEN='\e[4;32m'
UYELLOW='\e[4;33m'
UBLUE='\e[4;34m'
UMAGENTA='\e[4;35m'
UCYAN='\e[4;36m'
UWHITE='\e[4;37m'
# background colors
BBLACK='\e[40m'
BRED='\e[41m'
BGREEN='\e[42m'
BYELLOW='\e[43m'
BBLUE='\e[44m'
BMAGENTA='\e[45m'
BCYAN='\e[46m'
BWHITE='\e[47m'

PS1="\n\[$CYAN\][\[$NO_COLOR\]\u:\w\[$CYAN\]] \[$EBLACK\]\$(vcprompt)\[$NO_COLOR\] \[$CYAN\]\n→\[$NO_COLOR\] "

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

if [[ -s /Users/alx/.rvm/scripts/rvm ]] ; then
    source /Users/alx/.rvm/scripts/rvm ;
fi
