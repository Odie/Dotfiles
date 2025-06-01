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

# Load completions
autoload -U compinit && compinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

## Setting global ice for all subsequent plugins (resets at end of zshrc)
zinit ice depth=1; zinit light romkatv/powerlevel10k
zinit ice depth=1; zinit light zdharma-continuum/fast-syntax-highlighting    # Faster zsh syntax highlighting
zinit ice depth=1; zinit light zsh-users/zsh-completions                     # Adds completion definitions
zinit ice depth=1; zinit light zsh-users/zsh-autosuggestions                 # Suggest commands based on command history
zinit ice depth=1; zinit light Aloxaf/fzf-tab                                # Integrates the fzf fuzzy finder with Zsh's tab completion
zinit ice depth=1; zinit light joshskidmore/zsh-fzf-history-search           # Provides a way to search through command history using fzf
zinit ice depth=1; zinit light jeffreytse/zsh-vi-mode                        # Add vim mode for line editing
zinit ice depth=1; zinit light zsh-users/zsh-history-substring-search

zinit snippet OMZP::macos
zinit snippet OMZP::sudo
zinit snippet OMZP::command-not-found

# Load completions
autoload -U compinit && compinit

zinit cdreplay -q

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
setopt complete_in_word


# History search
function zvm_after_init() {
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
  bindkey "^p" history-substring-search-up
  bindkey "^n" history-substring-search-down
}

# Completion styling
# zstyle ':completion:*' completer _extensions _complete _approximate
# zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# zstyle ':completion:*' menu no
# zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'

# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
# NOTE: don't use escape sequences here, fzf-tab will ignore them
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
zstyle ':completion:*' menu no
# preview directory's content with eza when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
# switch group using `<` and `>`
zstyle ':fzf-tab:*' switch-group '<' '>'

# zstyle ':completion:*' menu select=1
# zstyle ':completion:*' use-compctl true
# zstyle ':completion:*' verbose true

# ------------------------ env initialization ----------------------------
export LC_CTYPE=C
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export VISUAL="nvim"
export EDITOR="${VISUAL} -f"
export MANPAGER="nvim +Man!"
# export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# FZF should respect gitignore settings
export FZF_DEFAULT_COMMAND="rg --smart-case --files --hidden --follow --glob '!.git'"

# ------------------------ fasd initialization ----------------------------
# eval "$(fasd --init auto)"
eval "$(zoxide init --cmd cd zsh)"

# Automatically activate a python venv at '.venv' if available
source $ZDOTDIR/auto_venv.zsh
source $ZDOTDIR/aliases.zsh

# ------------------------ zsh options  ----------------------------

# Accept 'dir' instead of 'cd dir'
setopt AUTO_CD

# Automatically pushd when we cd
setopt AUTO_PUSHD

# Import secrets
if [ -f "$HOME/.secrets.rc" ]; then
  source "$HOME/.secrets.rc"
fi

eval $(/opt/homebrew/bin/brew shellenv)
export PATH=$HOME/bin:$HOME/.local/bin:$PATH

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

# export PATH="$HOME/.tools/zig-0.11:$PATH"

# Mojo vars
export MODULAR_HOME="/Users/odie/.modular"
export PATH="/Users/odie/.modular/pkg/packages.modular.com_mojo/bin:$PATH"
export PATH="/Applications/Sublime Text.app/Contents/SharedSupport/bin/:$PATH"
export PATH="$PATH:/Users/odie/.cache/lm-studio/bin"

if command -v ngrok &>/dev/null; then
  eval "$(ngrok completion)"
fi
