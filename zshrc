# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="mikehale"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(gitfast ruby brew rake rbenv hk heroku gem)

source $ZSH/oh-my-zsh.sh
source $HOME/.zprofile

# Emacs shell shortcuts
function e()  { /usr/local/bin/emacs -nw $@ }
function ec() { /usr/local/bin/emacsclient -t $@ }
function es() { e --daemon=$1 && ec -s $1 }
function el() { psgrep -i emacs }
function ek() { /usr/local/bin/emacsclient -e '(kill-emacs)' -s $1 }
alias emacs=e

alias psgrep='psgrep -an'
alias ls='ls -h'
alias blog="cd $HOME/dev/blog"
alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'
alias rake_complete_clean='rm -rf ~/.raketabs'
alias kill_ssh="psgrep 'ssh ' | awk '{ print $2 }'| xargs kill"
alias tat='[ -z "${TMUX}" ] && (tmux attach || tmux new)'
alias which='nocorrect which'
alias dnsclearcache='dscacheutil -flushcache '
alias 'be=bundle exec'
alias wake-shuttle="wakeonlan 80:ee:73:04:58:9b"

function git(){hub "$@"}

# Heroku
alias hk='nocorrect hk'

function cdh {
  cd $HEROKU_HOME
}

function runtime_distribution {
  db update && db list runtime | grep us-east | awk '{print $7}'| sort | uniq -c
}

export DISABLE_CLOUD_SHOW=1
function _cloud_show() {
  [ -n "$HEROKU_CLOUD" ] && echo "HEROKU_CLOUD => $HEROKU_CLOUD"
  [ -n "$ION_HOST" ] && echo "ION_HOST => $ION_HOST"
  [ -n "$HEROKU_HOST" ] && echo "HEROKU_HOST => $HEROKU_HOST"
}

function cloud() {
  cloud="$1"
  if [ -z "$cloud" -o "$cloud" = "production" ]; then
     cloud="prod"
  fi

  export HEROKU_CLOUD="$cloud"
  echo "$HEROKU_CLOUD" > ~/.clouds/current

  if [ "$cloud" = "prod" ]; then
    unset ION_HOST ION_DOMAIN HEROKU_HOST
  elif [ "$cloud" = "shadow" ]; then
    export ION_HOST="ion-shadow.herokai.com"
    export HEROKU_HOST="heroku-shadow.com"
  elif [ "$cloud" = "ops" ]; then
    export ION_HOST="ion-ops.herokai.com"
    export HEROKU_HOST="ops.herokai.com"
  elif [ "$cloud" = "staging" ]; then
    export ION_HOST="ion-staging.herokai.com"
    export HEROKU_HOST="staging.herokudev.com"
  elif [ "$cloud" = "us-west-2" ]; then
    export ION_HOST="ion-us-west-2.herokai.com"
    export HEROKU_HOST="ion-us-west-2.herokudev.com"
  elif [ "$cloud" = "us-east-1-b" ]; then
    export ION_HOST="ion-us-east-1-b.herokai.com"
    export HEROKU_HOST="ion-us-east-1-b.herokudev.com"
  else
    export ION_HOST="ion-$cloud.herokuapp.com"
    export HEROKU_HOST="$cloud.herokudev.com"
  fi

  [ -n "$ION_HOST" ] && export ION_DOMAIN="$ION_HOST"

  unset USE_PUBLIC_IP
  unset EC2_PRIVATE_KEY
  unset EC2_CERT
  unset AWS_SSH_KEY
  unset AWS_ACCOUNT_ID
  unset AWS_ACCESS_KEY_ID
  unset AWS_SECRET_ACCESS_KEY
  unset AWS_CREDENTIAL_FILE
  unset AWS_DIR

  cloud_file="$HOME/.clouds/$cloud.sh"
  if [ -r "$cloud_file" ]; then
    . "$cloud_file"
    echo "Sourced $cloud_file"
  fi

  if [ -z "$DISABLE_CLOUD_SHOW" ]; then
    _cloud_show
  fi
}

cloud "$(< ~/.clouds/current)"

alias ion-client='TERM=xterm nocorrect ion-client'
alias ic='ion-client'
alias 'h=heroku'

function aws_set { 
  export AWS_DIR=$HOME/dev/heroku/secrets/aws/$1@heroku.com
  export EC2_PRIVATE_KEY=${AWS_DIR}/pk.pem
  export EC2_CERT=${AWS_DIR}/cert.pem

  if [ "$2" != "" ]; then
    export AWS_SSH_KEY=${AWS_DIR}/ssh-$2.pem
  else
    export AWS_SSH_KEY=${AWS_DIR}/ssh-default.pem
  fi

  source ${AWS_DIR}/env
}

function ssh_auth_socket_sync {
  export SSH_AUTH_SOCK=$(ls -1t /tmp/launch-*/Listeners|head -n 1)
}

ssh_auth_socket_sync

if [ -f "${HOME}/.gpg-agent-info" ]; then
  . "${HOME}/.gpg-agent-info"
  export GPG_AGENT_INFO
fi

if ! gpg-connect-agent 2>&1 >/dev/null </dev/null; then
  gpg-agent --daemon --write-env-file "${HOME}/.gpg-agent-info" >/dev/null
  . "${HOME}/.gpg-agent-info"
  export GPG_AGENT_INFO
fi

export GPG_TTY=$(tty)
export GPG_SIGNING_KEY="mhale@heroku.com"

pman () {
  man -t "${1}" | open -f -a /Applications/Preview.app
}
