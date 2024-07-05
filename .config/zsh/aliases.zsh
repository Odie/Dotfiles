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
