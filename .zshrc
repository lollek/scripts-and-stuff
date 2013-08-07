# Created 2013-03-07
# Last Update: 2013-05-25

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

unsetopt beep
setopt completeinword
bindkey -e # emacsy
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit


# Root PS1 is red. All others are green
if [ $UID -eq 0 ]; then
    PS1=$'[%T]%{\e[1;31m%}<%n%{\e[0m%}@%{\e[1;30m%}%m%{\e[0m%}%{\e[1;31m%}>%#%{\e[0m%} '
else 
    PS1=$'[%T]%{\e[1;32m%}<%n%{\e[0m%}@%{\e[1;30m%}%m%{\e[0m%}%{\e[1;32m%}>%#%{\e[0m%} '
fi  
  
# Always put pwd on the right side
RPS1=$'(%{\e[1;34m%}%~%{\e[0m%})'

# Easy update rc:
alias re-source-zsh='source ~/.zshrc'

# My stupid spotify username:
alias spotnick='echo "カッコいい人"'

# Think getpass():
alias getpass='read -s "passwd?Password: "; echo "Do not forget to passwd=\"\" later"'

# cd .. is annoying to type
alias ..='cd ..'

## ls
alias ls='/bin/ls --color=auto'
alias la='ls -A'
alias l='ls -lh --group-directories-first'
alias ll='ls -alh --group-directories-first'

## Verbosity is nice
alias ln='ln -v'
alias chmod='chmod -v'
alias chown='chown -v'
alias chgrp='chgrp -v'
alias mv='mv -v'
alias cp='cp -v'
alias rm='rm -v'

