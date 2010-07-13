# Use ~/.bashrc for things that you want around in subshells that wouldn't
# otherwise be inherited. So, aliases, completions, and functions.

alias CYAN='tput setaf 6'
alias MAGENTA='tput setaf 5'
alias RESET='tput sgr0'

alias couchdb_tunnel='ssh -f -N -L cauldron.sa2s.us -L5983:localhost:5983'
alias whatididyesterday='cat ~/sonian/worklog/`date -v -1d +%m-%d-%Y`.log'
alias psgrep='psgrep -an'
alias e='emacsclient'
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

function blade() {
 config=$1
 shift

 if [[ -f ~/.chef/platform/knife-$config.rb ]]; then
   knife "$@" -c ~/.chef/platform/knife-$config.rb
 else
   ls -la ~/.chef/platform/knife-$config.rb
 fi
}

# This file will source ~/.bash_completion
source ${HOMEBREW_HOME}/etc/bash_completion
source ${HOMEBREW_HOME}/Library/Contributions/brew_bash_completion.sh
source ${HOME}/.rvm/scripts/rvm
