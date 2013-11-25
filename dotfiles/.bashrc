#PS1='[\u@\h:\w]\$ '

if [[ $- != *i* ]] ; then 
  # Shell is non-interactive.
  return 
fi

# If ROOT; use red text
if [ $UID -ne 0 ]; then 
    PS1='\[\e[1;32m\][\u\[\e[m\]@\[\e[1;30m\]\h:\[\e[1;34m\]\w\[\e[1;32m\]]\$\[\e[0m\] '
else 
    PS1='\[\e[1;31m\][\u\[\e[m\]@\[\e[1;30m\]\h:\[\e[1;34m\]\w\[\e[1;31m\]]\$\[\e[0m\] '
fi     

# My stupid spotify username:
alias spotnick='echo "カッコいい人"'

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

# screen
alias s-rd='screen -rd'
alias s-ls='screen -ls'

# other
alias mc='. /usr/share/mc/bin/mc-wrapper.sh'
alias j='jobs -l'
alias re-source-bash='source ~/.bashrc'
alias sshkey='ssh-keygen -l -f /etc/ssh/ssh_host_ecdsa_key.pub'
alias se='setxkbmap -layout se'
alias us='setxkbmap -layout us'

# Speak is kinda funny
alias speak='spd-say -e -t male3 -r -30 -p -100'
