# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="odie"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# ------------------------ env initialization ----------------------------
export PATH="/Users/Odie/.rbenv/shims:${PATH}"
export TERM='xterm-256color'

export LC_CTYPE=C
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

#export PATH=~/.lua/bin:~/.luarocks/bin:/Users/Odie/Library/Python/2.7/bin:/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin:~/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/libexec:$PATH
export PATH=/Users/Odie/Library/Python/2.7/bin:/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin:~/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/libexec:$PATH
#export EDITOR="subl -w"
export EDITOR="vim -f"

# ------------------------ lua env initialization ----------------------------
#LUA_PATH="./?.lua;~/.lua/bin/?.lua;~/.luarocks/share/lua/5.1/?.lua;~/.luarocks/share/lua/5.1/?/init.lua;/opt/local/share/lua/5.1/?.lua;/opt/local/share/lua/5.1/?/init.lua;/opt/local/lib/lua/5.1/?.lua;/opt/local/lib/lua/5.1/?/init.lua"
#LUA_CPATH='./?.so;~/.lua/lib/?.so;~/.luarocks/lib/lua/5.1/?.so'
#export LUA_PATH=${LUA_PATH//'~'/$HOME}
#export LUA_CPATH=${LUA_CPATH//'~'/$HOME}

# ------------------------ rbenv initialization ----------------------------
eval "$(rbenv init -)"

# ------------------------ python initialization ----------------------------
export PYTHONPATH=`brew --prefix`/lib/python2.7/site-packages:$PYTHONPATH

# ------------------------ ansible initialization ----------------------------
export ANSIBLE_HOSTS=~/.ansible_hosts

# ------------------------ powerline initialization ----------------------------
. $HOME/.janus/powerline/powerline/bindings/zsh/powerline.zsh

# ------------------------ fasd initialization ----------------------------
eval "$(fasd --init auto)"

# ------------------------ commandline utils  ----------------------------
export PATH="/Users/Odie/.rbenv/shims:${PATH}"

alias ls="ls -G" # list
alias la="ls -Ga" # list all, includes dot files
alias ll="ls -Glh" # long list, excludes dot files
alias lla="ls -Gla" # long list all, includes dot files

alias v='f -e vim' # quick opening files with vim
alias 'json'='python -mjson.tool'

alias ips="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print $1'"
alias flush="dscacheutil -flushcache" # Flush DNS cache
alias fp="cd \`finderPath\`"

function swap()
{
	tmpfile=$(mktemp $(dirname "$file1")/XXXXXX)
	mv "$1" "$tmpfile"
	mv "$2" "$1"
	mv "$tmpfile" "$2"
}

function finderPath()
{
	osascript -e 'tell application "Finder"'\
	-e "if (${1-1} <= (count Finder windows)) then"\
	-e "get POSIX path of (target of window ${1-1} as alias)"\
	-e 'else' -e 'get POSIX path of (desktop as alias)'\
	-e 'end if' -e 'end tell';
}
