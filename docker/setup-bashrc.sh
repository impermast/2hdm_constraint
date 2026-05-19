#!/usr/bin/env bash
set -euo pipefail

USER_NAME="${1:-worker}"
USER_HOME="/home/${USER_NAME}"
BASHRC="${USER_HOME}/.bashrc"

cat > "${BASHRC}" <<'BASHRC_EOF'
# ~/.bashrc for 2HDM Mathematica workstation

# Do nothing for non-interactive shells.
case $- in
  *i*) ;;
  *) return ;;
esac

# History.
HISTCONTROL=ignoreboth
HISTSIZE=5000
HISTFILESIZE=10000
shopt -s histappend
shopt -s checkwinsize

# Friendly less.
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Project environment.
export TWOHDM_ROOT="${TWOHDM_ROOT:-/home/worker/2hdm}"
export WOLFRAM_USERBASE="${WOLFRAM_USERBASE:-/opt/wolfram/userbase}"
export MATHEMATICA_USERBASE="${MATHEMATICA_USERBASE:-/opt/wolfram/userbase}"
export LHAPDF_PREFIX="${LHAPDF_PREFIX:-/opt/lhapdf}"
export LHAPDF_DATA_PATH="${LHAPDF_DATA_PATH:-/opt/lhapdf/share/LHAPDF}"
export PATH="/usr/local/bin:/opt/lhapdf/bin:/opt/wolfram/userbase/Applications/LoopTools/bin:${PATH}"
export LD_LIBRARY_PATH="/opt/lhapdf/lib:${LD_LIBRARY_PATH:-}"

# Completion.
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Colored prompt.
case "$TERM" in
  xterm-color|*-256color) color_prompt=yes ;;
esac

if [ "${color_prompt:-}" = yes ]; then
  PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
  PS1='\u@\h:\w\$ '
fi
unset color_prompt

case "$TERM" in
  xterm*|rxvt*) PS1="\[\e]0;\u@\h: \w\a\]$PS1" ;;
esac

# Colors and tools.
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ls='ls --color=auto --group-directories-first'
alias ll='ls -lah --color=auto --group-directories-first'
alias la='ls -A --color=auto --group-directories-first'
alias l='ls -CF --color=auto'
alias cls='clear'
alias path='echo -e ${PATH//:/\\n}'

if command -v rg >/dev/null 2>&1; then
  alias grep='rg --color=always --line-number --hidden --smart-case'
else
  alias grep='grep --color=auto'
fi

if command -v batcat >/dev/null 2>&1; then
  alias cat='batcat --theme="TwoDark" --style=plain --paging=never'
  alias bat='batcat'
fi

if command -v fdfind >/dev/null 2>&1; then
  alias fd='fdfind'
fi

# Git.
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gd='git diff'
alias gl='git log --oneline --graph --decorate --all -20'
alias gp='git pull --ff-only'

# Project shortcuts.
alias c2='cd /home/worker/2hdm'
alias cz='cd /home/worker/2hdm/ZZZ_ZWW'
alias cm='cd /home/worker/2hdm/modules'
alias cb='cd /home/worker/2hdm/ZZZ_ZWW/buffer'

# Wolfram shortcuts.
alias mathgui='mathematica'
alias mscript='wolframscript'
alias mtest='wolframscript -code "Print[1+1]"'

# Package manager helpers.
alias install='sudo apt-get install'
alias update='sudo apt-get update'

# Start in the project directory.
cd /home/worker/2hdm 2>/dev/null || cd /home/worker
BASHRC_EOF

chown "${USER_NAME}:${USER_NAME}" "${BASHRC}"
