# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# ------------------------ env initialization ----------------------------
export TERM='xterm-256color'

export LC_CTYPE=C
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export PATH=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin:~/bin:~/.composer/vendor/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/libexec:$PATH
export EDITOR="vim -f"
export VISUAL="vim"

# ------------------------ fasd initialization ----------------------------
eval "$(fasd --init auto)"

# ------------------------ commandline utils  ----------------------------
alias ls="ls -G" # list
alias la="ls -Ga" # list all, includes dot files
alias ll="ls -Glh" # long list, excludes dot files
alias lla="ls -Gla" # long list all, includes dot files

alias v='f -e vim' # quick opening files with vim
alias 'json'='python -mjson.tool'

alias ips="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print $1'"
alias flush="dscacheutil -flushcache" # Flush DNS cache

alias dh="dirs -v"

function phpunit()
{
	`git-root`/vendor/bin/phpunit -c `git-root`/phpunit.xml
}

# ------------------------ zsh options  ----------------------------

# Accept 'dir' instead of 'cd dir'
setopt AUTO_CD

# Automatically pushd when we cd
setopt AUTO_PUSHD
