source ~/.profile

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# ------------------------ env initialization ----------------------------
export LC_CTYPE=C
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# export PATH=~/.cargo/bin:/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin:~/bin:~/.composer/vendor/bin:~/.rbenv/shims:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/libexec:$PATH
export VISUAL="nvim"
export EDITOR="${VISUAL} -f"

# FZF should respect gitignore settings
export FZF_DEFAULT_COMMAND="rg --smart-case --files --hidden --follow --glob '!.git'"

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

alias v='eval $FZF_DEFAULT_COMMAND | fzf | xargs nvim'

function fzf_jump_cd() {
  if [[ -z "$*" ]]; then
    cd "$(fasd_cd -Rdl | fzf --no-sort | sed 's/^[0-9,.]* *//')"
  else
    fasd_cd -d "$@"
  fi
}

alias z=fzf_jump_cd
alias j=fzf_jump_cd

alias gcob='git branch | fzf | xargs git checkout'
alias rg='rg --smart-case --follow --hidden --glob "!.git"'
# ------------------------ zsh options  ----------------------------

# Accept 'dir' instead of 'cd dir'
setopt AUTO_CD

# Automatically pushd when we cd
setopt AUTO_PUSHD

# Added by ~/.emacs.d/install.sh
export PATH=$HOME/.cask/bin:$PATH:$HOME/.vim/plugged/vim-iced/bin
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export JAVA_HOME=$(/usr/libexec/java_home)
export M2_HOME=`brew --prefix maven`/libexec
export M2=`brew --prefix maven`/libexec/bin
export HOMEBREW_GITHUB_API_TOKEN="3819bdf17e20a7b5f2068246920e90d0428547a4"

export CLOJARS_USER=""
export CLOJARS_PASS=""
export BOOT_JVM_OPTIONS="-XX:-OmitStackTraceInFastThrow -Xverify:none"
export GRAALVM_HOME="/Library/Java/JavaVirtualMachines/graalvm-ce-19.2.1/Contents/Home"

# export PATH=$GRAALVM_HOME/bin:"$PATH"
#export PATH=/usr/local/anaconda3/bin:"$PATH"
#export PATH="$HOME/.jenv/bin:$PATH"
#eval "$(jenv init -)"
export PATH="/usr/local/opt/llvm/bin:$PATH"
