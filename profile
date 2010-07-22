# vim: set filetype=sh :

# Use ~/.profile for things that can be inherited by subshells. As far as I
# know right now, that just means environment variables.

# histappend and HIST* envars from
# http://blog.macromates.com/2008/working-with-history-in-bash/
shopt -s histappend

export HOMEBREW_HOME="$(brew --prefix)"
export CLICOLOR=yes
export GIT_PS1_SHOWDIRTYSTATE=yes
export GIT_PS1_SHOWSTASHSTATE=yes
export GIT_PS1_SHOWUNTRACKEDFILES=yes
export GREP_COLOR='30;102'
export GREP_OPTIONS='--color'
export HISTCONTROL=erasedups
export HISTSIZE=1000
# LESS settings ganked from git (see core.pager in git-config(1))
# Used here because they're also convenient for ri.
export LESS='--quit-if-one-screen --RAW-CONTROL-CHARS --chop-long-lines --no-init'
export PATH="${HOME}/.bin:${HOMEBREW_HOME}/bin:${HOMEBREW_HOME}/sbin:${HOME}/.rvm/bin:${PATH}:/Library/Application Support/VMware Fusion"
export PS1='\[$(CYAN)\]$(__rvm_ps1)$(__bundler_ps1)\[$(RESET)\]\w\[$(MAGENTA)\]$(__git_ps1)\[$(RESET)\] '
export RI='--format ansi'
export RSYNC_RSH='ssh'
export EDITOR="DISPLAY= emacsclient -a '' -c -s server"
export GEMEDITOR=mate
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home/

if [ -n "${SSH_AUTH_SOCK:-x}" ] ; then
  export SSH_AUTH_SOCK=`ls /tmp/launch-*/Listeners`
fi

# On OSX make sure TERM is set
export TERM=xterm

# Add AWS tools
export EC2_HOME="${HOMEBREW_HOME}/Cellar/ec2-api-tools/1.3-46266"
export AWS_CLOUDWATCH_HOME="${HOMEBREW_HOME}/Cellar/cloud-watch/1.0.2.3"
export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"
export AWS_AUTO_SCALING_HOME="${HOMEBREW_HOME}/Cellar/auto-scaling/1.0.9.0"
export AWS_ELB_HOME="${HOMEBREW_HOME}/Cellar/elastic-load-balancing/1.0.3.4"

export CAULDRON_KEY="$HOME/sonian/keys/ssh/cauldron.pem"

function set_ec2_keys {
  key_name=$1 
  export EC2_CERT="$HOME/.ec2/cert-$key_name.pem"
  export EC2_PRIVATE_KEY="$HOME/.ec2/pk-$key_name.pem"
}
source "$HOME/.ec2/defaults"

case "$TERM" in
xterm*|rxvt*)
  # I'd like to use tput here as well, but my terminal doesn't support it.
  # http://serverfault.com/questions/23978/how-can-one-set-a-terminals-title-with-the-tput-command
  # export PS1='$(tput tsl)\W$(tput fsl)'$PS1
  export PS1="\[\e]0;\W\a\]"$PS1
  ;;
*)
  ;;
esac

if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi
