# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [[ -f ~/.profile ]]; then
  source ~/.profile
fi

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

# Start zinit
export ZINIT_HOME="${XDA_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
source "${ZINIT_HOME}/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

zinit ice depth=1; zinit light romkatv/powerlevel10k
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab
zinit light joshskidmore/zsh-fzf-history-search

# Load completions
autoload -U compinit && compinit

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh


# History
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# History search
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search
bindkey "^p" history-search-backward
bindkey "^n" history-search-forward

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'


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
# eval "$(fasd --init auto)"
eval "$(zoxide init --cmd cd zsh)"

# ------------------------ commandline utils  ----------------------------
alias ls="eza" # list
alias la="eza -a" # list all, includes dot files
alias ll="eza -lh" # long list, excludes dot files
alias lla="eza -lha" # long list all, includes dot files

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
alias git-root="git rev-parse --show-toplevel"

function grt()
{
  local root_dir

  # Capture the output of the git-root alias
  root_dir=$(git-root 2>/dev/null)

  # Check if the git-root command was successful
  if [ -n "$root_dir" ]; then
      cd "$root_dir"
  else
      echo "Error: Current directory is not inside a Git repository."
  fi
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

  if [[ ! -d ".git" ]]; then
    echo "Initializing a new Git repository..."
    git init
  else
    echo "Git repository already exists."
  fi

  autoactivate_python_venv
}


VENV_CURRENT=""
function autoactivate_python_venv() {
  # Grab the path to the current git project
  local PROJ=`git-root 2> /dev/null` || ""

  # If we are no longer in a project and venv seems active, deactivate now
  if [[ -z $PROJ ]]; then
  	if [[ -d $VENV_CURRENT ]]; then
  	  deactivate > /dev/null 2>&1
  	  VENV_CURRENT=""
  	fi
    if [[ -n $CONDA_PREV_ENV ]]; then
      conda activate $CONDA_PREV_ENV
      CONDA_PREV_ENV=""
    fi

  	return
  fi

  # We are in a git project, let's see if we can find a venv directory
  local ENVDIR=$PROJ/$VENV_DEFAULT

  # If a venv directory is present...
  if [[ -d $ENVDIR ]]; then
    CONDA_PREV_ENV=$CONDA_DEFAULT_ENV
    if [[ -n CONDA_DEFAULT_ENV ]]; then
      conda deactivate
    fi
  	if [[ $ENVDIR != $VENV_CURRENT ]]; then
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
add-zsh-hook chpwd autoactivate_python_venv

alias activate_venv=autoactivate_python_venv

# ------------------------ Faster navigation ----------------------------
# Currently, things are implemented using fasd + fzf
# -----------------------------------------------------------------------

alias v='eval $FZF_DEFAULT_COMMAND | fzf | xargs -I {} nvim "{}"'

function fzf_jump_cd() {
  if [[ -z "$*" ]]; then
    cd "$(zoxide query -l | fzf --no-sort | sed 's/^[0-9,.]* *//')"
  else
    cd "$(zoxide query $@)"
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

# Import secrets
if [ -f "$HOME/.secrets.rc" ]; then
  source "$HOME/.secrets.rc"
fi

# Added by ~/.emacs.d/install.sh
export PATH=$HOME/bin:$HOME/.local/bin:$HOME/.cask/bin:/opt/homebrew/bin:$PATH
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(fzf --zsh)"

export JAVA_HOME=$(/usr/libexec/java_home)
export M2_HOME=`brew --prefix maven`/libexec
export M2=`brew --prefix maven`/libexec/bin

export CLOJARS_USER=""
export CLOJARS_PASS=""
export BOOT_JVM_OPTIONS="-XX:-OmitStackTraceInFastThrow -Xverify:none"
export GRAALVM_HOME="/Library/Java/JavaVirtualMachines/graalvm-ce-19.2.1/Contents/Home"

# export PATH=$GRAALVM_HOME/bin:"$PATH"
# export PATH=/usr/local/anaconda3/bin:"$PATH"
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
export PATH="/usr/local/opt/llvm/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/odie/.tools/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/odie/.tools/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/odie/.tools/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/odie/.tools/google-cloud-sdk/completion.zsh.inc'; fi

source /Users/odie/.docker/init-zsh.sh || true # Added by Docker Desktop

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

activate_venv

# pnpm
export PNPM_HOME="/Users/odie/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

export PATH="$HOME/.tools/zig-0.11:$PATH"

# Mojo vars
export MODULAR_HOME="/Users/odie/.modular"
export PATH="/Users/odie/.modular/pkg/packages.modular.com_mojo/bin:$PATH"

