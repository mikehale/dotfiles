# setup homebrew

brew install tmux

# fix pbcopy
brew install reattach-to-user-namespace --wrap-launchctl --wrap-pbcopy-and-pbpaste

# support dired
brew install coreutils

# fonts

cp fonts/AnonymousPro-1.002.001/*.ttf ~/Library/Fonts

# terminal info

tic -o ~/.terminfo /usr/local/Cellar/emacs/24.3/share/emacs/24.3/etc/e/eterm-color.ti
