# Use ~/.zprofile for things that can be inherited by subshells. As far as I
# know right now, that just means environment variables.

export HOMEBREW_HOME="$(/usr/local/bin/brew --prefix)"
export CLICOLOR=yes
export GIT_PS1_SHOWDIRTYSTATE=yes
export GIT_PS1_SHOWSTASHSTATE=yes
export GIT_PS1_SHOWUNTRACKEDFILES=yes
export GREP_COLOR='30;102'
export GREP_OPTIONS='--color'
# LESS settings ganked from git (see core.pager in git-config(1))
# Used here because they're also convenient for ri.
export LESS='--quit-if-one-screen --RAW-CONTROL-CHARS --chop-long-lines --no-init'

# PATH
export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin"
export PATH="${HOME}/.bin:${HOMEBREW_HOME}/bin:${HOMEBREW_HOME}/sbin:${PATH}:/Library/Application Support/VMware Fusion:/usr/local/CrossPack-AVR-20120217/bin"
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
export PATH="/usr/local/heroku/bin:/usr/local/foreman/bin:$PATH"

export RI='--format ansi'
export RSYNC_RSH='ssh'
export DISPLAY=''
export ALTERNATIVE_EDITOR=""
export EDITOR="emacsclient -t"

export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home/
export NODE_PATH=$NODE_PATH:${HOMEBREW_HOME}/lib/node_modules

if [ -n "${SSH_AUTH_SOCK:-x}" ] ; then
  export SSH_AUTH_SOCK=$(ls -t /tmp/launch-*/Listeners|tail -n1)
fi

# # On OSX make sure TERM is set
# if [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
#     export TERM=xterm-color
# fi
# if [ "$TERM_PROGRAM" = "iTerm.app" ]; then
#     export TERM=xterm-256color
# fi

# # Go Stuff
export GOROOT=`brew --cellar`/go/r60.3
export GOBIN=/usr/local/bin
export GOARCH=amd64
export GOOS=darwin

# # Add AWS tools
export EC2_HOME="${HOMEBREW_HOME}/Library/LinkedKegs/ec2-api-tools/jars"
export AWS_CLOUDWATCH_HOME="${HOMEBREW_HOME}/Cellar/cloud-watch/1.0.12.1/jars"
export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"
export AWS_AUTO_SCALING_HOME="${HOMEBREW_HOME}/Library/LinkedKegs/auto-scaling/jars"
export AWS_ELB_HOME="${HOMEBREW_HOME}/Cellar/elb-tools/1.0.14.3/jars"
export AWS_RDS_HOME="${HOMEBREW_HOME}/Cellar/rds-command-line-tools/1.2.006/jars"

# Heroku
export HEROKU_HOME=$HOME/dev/heroku

# if [[ -z "$TMUX" ]]; then
#   tmux attach || tmux
# fi
