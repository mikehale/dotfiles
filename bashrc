# Use ~/.bashrc for things that you want around in subshells that wouldn't
# otherwise be inherited. So, aliases, completions, and functions.

alias coffee='countdown 5 "The coffee should be ready."'
alias water='countdown 42 "The water should be boiling by now."'

alias g='gem-open'
alias git='RUBYOPT= hub'
alias ls='ls -h'
alias migrate='rake db:migrate db:test:prepare'
alias rails='generate-rails-app'
alias sc='script/console'
alias ss='script/server'
alias v=mvim
alias wip='rake cucumber:wip'

function __bundler_ps1 {
  if [ -n "${BUNDLE_GEMFILE-}" ]; then
    project_path="${BUNDLE_GEMFILE%/Gemfile}"
    project_name="${project_path##**/}"

    if [ -n "${1-}" ]; then
      printf "$1" "${project_name}"
    else
      printf " (%s)" "${project_name}"
    fi
  fi
}

function p {
  cd ~/Code/$1; ls
}

function pi {
  cd ~/Code/.inactive/$1; ls
}

if [ -f `brew --prefix`/etc/bash_completion ]; then
  . `brew --prefix`/etc/bash_completion
fi

if [ -f `brew --prefix`/Library/Contributions/brew_bash_completion.sh ]; then
  . `brew --prefix`/Library/Contributions/brew_bash_completion.sh
fi

complete -W 'ls refresh start stop' downloads
complete -C 'gem-complete' g
complete -C "directory-complete ${HOME}/Code" p
complete -C "directory-complete ${HOME}/Code/.inactive" pi
complete -C 'rake-complete' rake
