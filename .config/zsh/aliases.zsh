# -----------------------------------------------------------------------
# Plain Aliases 
# -----------------------------------------------------------------------

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


# -----------------------------------------------------------------------
# Commandline Utils
# -----------------------------------------------------------------------
function grt()
{
  local root_dir

  # Capture the output of the git-root alias
  root_dir=$(git-root 2>/dev/null)

  # Check if the git-root command was successful
  if [ -n "$root_dir" ]; then
      cd "$root_dir"
  else
      echo "Error: Current directory is not inside a Git repository." >&2
      return 1
  fi
}

function phpunit()
{
	`git-root`/vendor/bin/phpunit -c `git-root`/phpunit.xml
}

function yy() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		cd "$cwd"
	fi
	rm -f -- "$tmp"
}

__zoxide_fzf_cd () {
  if [[ "$#" -eq 0 ]]
  then
    # Change directory to the home directory
    __zoxide_cd ~
  elif [[ "$#" -eq 1 ]] && {
      # Check if the argument is a directory, the dash character, or a numeric argument with optional + or - prefix
      [[ -d "$1" ]] || [[ "$1" = '-' ]] || [[ "$1" =~ ^[-+][0-9]$ ]]
  }
  then
    # Change directory to the argument
    __zoxide_cd "$1"
  else
    # Try to use fzf to figure out the best match child dreitory using fzf

    # Declare a local variable 'result'
    \builtin local result
    
    # Get the current directory
    current_dir="$(__zoxide_pwd)"
    
    # List all current child directories
    # Use fzf to select the best match using the supplied parameter as the filter criteria
    result=$(find . -mindepth 1 -maxdepth 1 -type d | fzf --filter="$*" | head -n 1)
    
    echo "$result"
    # If a result is found, change directory to the result
    if [[ -n "$result" ]]; then
        __zoxide_cd "$result"
    else
        echo "No matching directory found."
    fi
  fi
}

cd () {
        __zoxide_fzf_cd "$@"
}
