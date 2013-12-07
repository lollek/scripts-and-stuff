## Created 2013-09-13
## Updated 2013-10-06
## Olle K zshrc

if [[ $- != *i* ]]; then
  return
fi

zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit && compinit
autoload -U promptinit && promptinit

## See man zshoptions
# Completion
setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_MENU           # Show completion menu on a succesive tab press.
setopt AUTO_PARAM_SLASH    # If parameter is a directory, add a trailing slash.
setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
# Expansion and globbing
unsetopt CASE_GLOB         # Make globbing case insensitive
setopt EXTENDEDGLOB        # Glob # ^ ~
# History
setopt APPENDHISTORY       # Add to hist instead of replacing it
setopt HIST_IGNORE_DUPS    # Don't add duplicates to hist
# Input / Output
unsetopt FLOW_CONTROL      # Disable XON/XOFF characters in shell editor.
setopt PATH_DIRS           # Path search even on command names with slashes.
# Job control
setopt NOTIFY              # Report background job status immediately
# Emacs-style
bindkey -e
setopt nomatch 

# Use caching to make completion for cammands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
VISUAL=vim
EDITOR=vim

# PS1
case `uname -n` in
  "localhost") 
    prompt adam2
    ;;
  "kilo")
    prompt clint
    export LANG=en_US.utf8
    ;;
  *)
  prompt clint
  ;;
esac

# Encoding
if [[ $LANG != "en_US.UTF-8" && $LANG != "en_US.utf8" ]]; then
  echo "Your encoding seems off? check 'locale charmap' and the 'export LANG='"
fi

# Own Commands
if [[ -d ~/bin ]]; then
  export PATH=~/bin:$PATH
fi

alias ..='cd ..'
alias chmod='chmod -v'
alias chown='chown -v'
alias chgrp='chgrp -v'
alias cp='cp -v'
alias ln='ln -v'
alias mv='mv -v'
alias rm='rm -v'
alias ls='ls --color=auto --group-directories-first'
alias la='ls -A'
alias l='ls -lh'
alias ll='ls -alh'

if [[ `uname` == SunOS ]]; then
  alias ls='ls --color=auto'
  alias emacs='emacs --color=always'
fi

# Compiling
alias gcc='gcc -Wall -Wextra -Werror -pedantic -g'
alias g++='g++ -Wall -Wextra -Werror -pedantic -Weffc++ -g'
alias g++11='g++ -std=c++11'
alias clang++11='clang++ -std=c++11'
