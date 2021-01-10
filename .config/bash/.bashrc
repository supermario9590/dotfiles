#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\u@\h \W \$ '
export EMAIL=anishreddyvundela@gmail.com

if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ] ;
  then PATH="$HOME/.cabal/bin:$PATH"
fi

alias ls='exa -al'

# Vim Keys
alias :q='exit'
