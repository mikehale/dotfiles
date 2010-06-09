# Use ~/.bashrc for things that you want around in subshells that wouldn't
# otherwise be inherited. So, aliases, completions, and functions.

alias CYAN='tput setaf 6'
alias MAGENTA='tput setaf 5'
alias RESET='tput sgr0'

alias psgrep='psgrep -an'
alias ls='ls -h'
alias blog="cd $HOME/dev/blog"
alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'
alias rake_complete_clean='rm -rf ~/.raketabs'
alias mh="cd ~/dev/memberhub/application"
# clear RUBYOPT when calling hub
alias git='RUBYOPT= hub'

function __bundler_ps1 {
  if [ -n "${BUNDLE_GEMFILE-}" ]; then
    printf "${1-(%s) }" "$(dirname $BUNDLE_GEMFILE | xargs basename)"
  fi
}

function __rvm_ps1 {
  if [ $(which ruby) != '/usr/bin/ruby' ]; then
    printf "${1-(%s) }" "$(rvm gemdir | xargs basename)"
  fi
}

function pg {
  cd $(gem-directory $1); ls
}

function pr {
  cd `ruby -rrbconfig -e 'puts Config::CONFIG["rubylibdir"]'`; ls
}

# This file will source ~/.bash_completion
source ${HOMEBREW_HOME}/etc/bash_completion
source ${HOMEBREW_HOME}/Library/Contributions/brew_bash_completion.sh
source ${HOME}/.rvm/scripts/rvm
