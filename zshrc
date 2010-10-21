# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
# export ZSH_THEME="robbyrussell"
export ZSH_THEME="mikehale"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ruby brew gem)

source $ZSH/oh-my-zsh.sh


alias psgrep='psgrep -an'
alias e="$EDITOR"
alias ls='ls -h'
alias blog="cd $HOME/dev/blog"
alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'
alias rake_complete_clean='rm -rf ~/.raketabs'
# clear RUBYOPT when calling hub
alias git='RUBYOPT= hub'
alias tmux_main='tmux attach -t main || tmux new -s main'
alias kill_ssh="psgrep 'ssh ' | awk '{ print $2 }'| xargs kill"
alias ec='emacsclient'

source ${HOME}/.rvm/scripts/rvm

function blade() {
 config=$1
 shift

 if [[ -f ~/.chef/platform/knife-$config.rb ]]; then
   knife "$@" -c ~/.chef/platform/knife-$config.rb
 else
   ls -la ~/.chef/platform/knife-$config.rb
 fi
}
