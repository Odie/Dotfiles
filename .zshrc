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
alias ls="exa" # list
alias la="exa -a" # list all, includes dot files
alias ll="exa -lh" # long list, excludes dot files
alias lla="exa -lha" # long list all, includes dot files

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

VENV_DEFAULT=".venv"
function mkvenv() {
  local VENV_NAME="${1:-$VENV_DEFAULT}"
  echo "Creating venv in $VENV_NAME"
  PY3=`which python3`
  $PY3 -m venv ${VENV_NAME}
}


VENV_CURRENT=""
function python_venv() {
  # Grab the path to the current git project
  local PROJ=`git-root 2> /dev/null` || ""

  # If we are no longer in a project and venv seems active, deactivate now
  if [[ -z $PROJ ]] then
  	if [[ -d $VENV_CURRENT ]] then
  	  deactivate > /dev/null 2>&1
  	  VENV_CURRENT=""
  	fi
  	return
  fi

  # We in a git project, let's see if we can find a venv directory
  local ENVDIR=$PROJ/$VENV_DEFAULT

  # If a venv directory is present...
  if [[ -d $ENVDIR ]] then
  	if [[ $ENVDIR != $VENV_CURRENT ]] then
  	  # Deactivate the venv we're currently in
  	  [[ -d $VENV_CURRENT ]] && deactivate > /dev/null 2>&1

  	  # Activate the new venv
   	  source $ENVDIR/bin/activate > /dev/null 2>&1
  	fi

   	# Record the currently active venv
   	VENV_CURRENT=$ENVDIR
  fi
}

autoload -U add-zsh-hook
add-zsh-hook chpwd python_venv

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

if [ -f "~/.secrets.rc" ]; then
  source "~/.secrets.rc"
fi

# Added by ~/.emacs.d/install.sh
export PATH=$HOME/bin:$HOME/.cask/bin:$PATH:$HOME/.vim/plugged/vim-iced/bin
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export JAVA_HOME=$(/usr/libexec/java_home)
export M2_HOME=`brew --prefix maven`/libexec
export M2=`brew --prefix maven`/libexec/bin

export CLOJARS_USER=""
export CLOJARS_PASS=""
export BOOT_JVM_OPTIONS="-XX:-OmitStackTraceInFastThrow -Xverify:none"
export GRAALVM_HOME="/Library/Java/JavaVirtualMachines/graalvm-ce-19.2.1/Contents/Home"

# export PATH=$GRAALVM_HOME/bin:"$PATH"
#export PATH=/usr/local/anaconda3/bin:"$PATH"
#export PATH="$HOME/.jenv/bin:$PATH"
#eval "$(jenv init -)"
export PATH="/usr/local/opt/llvm/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/Odie/.tools/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/Odie/.tools/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/Odie/.tools/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/Odie/.tools/google-cloud-sdk/completion.zsh.inc'; fi
