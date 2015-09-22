# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# ------------------------ env initialization ----------------------------
export LC_CTYPE=C
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export PATH=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin:~/bin:~/.composer/vendor/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/libexec:$PATH
export VISUAL="nvim"
export EDITOR="${VISUAL} -f"

# FZF should respect gitignore settings
export FZF_DEFAULT_COMMAND='ag -l -g ""'

# ------------------------ fasd initialization ----------------------------
eval "$(fasd --init auto)"

# ------------------------ commandline utils  ----------------------------
alias ls="ls -G" # list
alias la="ls -Ga" # list all, includes dot files
alias ll="ls -Glh" # long list, excludes dot files
alias lla="ls -Gla" # long list all, includes dot files

alias vim="nvim"
alias vi="nvim"
alias em='emacs'
alias 'json'='python -mjson.tool'

alias ips="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print $1'"
alias dnsflush="sudo killall -HUP mDNSResponder" # Flush DNS cache

alias dh="dirs -v"

alias art="artisan"
alias phpspec="vendor/bin/phpspec"
alias codecept="vendor/bin/codecept"

function grt()
{
	cd `git-root`
}

function phpunit()
{
	`git-root`/vendor/bin/phpunit -c `git-root`/phpunit.xml
}

# ------------------------ Faster navigation ----------------------------
# Currently, things are implemented using fasd + fzf
# -----------------------------------------------------------------------

function v() {
	local file
	file=$(ag -l -g "" | fzf) && ${VISUAL} ${file}
}

unalias j
function j() {
  if [[ -z "$*" ]]; then
    cd "$(fasd_cd -Rdl | fzf --no-sort | sed 's/^[0-9,.]* *//')"
  else
    fasd_cd -d "$@"
  fi
}

unalias z
alias z=j

# ------------------------ zsh options  ----------------------------

# Accept 'dir' instead of 'cd dir'
setopt AUTO_CD

# Automatically pushd when we cd
setopt AUTO_PUSHD

# Base16 Shell
BASE16_SCHEME="railscasts"
BASE16_SHELL="$HOME/.config/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL


# Added by ~/.emacs.d/install.sh
export PATH=$HOME/.cask/bin:$PATH
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export JAVA_HOME=$(/usr/libexec/java_home)
export M2_HOME=`brew --prefix maven`/libexec
export M2=`brew --prefix maven`/libexec/bin
