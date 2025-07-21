alias ls="eza" # list
alias la="eza -a" # list all, includes dot files
alias ll="eza -lh" # long list, excludes dot files
alias lla="eza -lha" # long list all, includes dot files
alias ltg="eza -lha --tree --git-ignore"
alias lt="eza -lha --tree"

alias vim="nvim"
alias vi="nvim"
alias em='emacs'
alias 'json'='python -mjson.tool'

alias ips="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print $1'"
alias dnsflush="sudo killall -HUP mDNSResponder" # Flush DNS cache

alias rg='rg --smart-case --follow --hidden --glob "!.git"'
alias llm="uvx llm"

#############################################################################
# Open a file in vim quickly with fzf                                       #
#############################################################################
function v
    if test (pwd) = $HOME
        echo "⚠️  Refusing to run from ~ (too many files)" >&2
        return 1
    end
    set -l search_cmd (default $FZF_DEFAULT_COMMAND 'fd .')
    set file (eval $search_cmd | fzf --preview='bat --style=numbers --color=always --line-range=:500 {}' --height=80%)
    if test -n "$file"
        nvim "$file"
    end
end

#############################################################################
# Jump to git root                                                          #
#############################################################################
alias git-root="git rev-parse --show-toplevel"
function grt
    # set root_dir (git-root ^/dev/null)
    set root_dir (git-root 2>/dev/null)
    if test -n "$root_dir"
        cd "$root_dir"
    else
        echo "Error: Current directory is not inside a Git repository." >&2
        return 1
    end
end

#############################################################################
# Faster jumping to frequently used directories                             #
#############################################################################
function fzf_jump_cd
    # If no params were passed, then show zoxide known items by frequency in fzf for selection
    if test (count $argv) -eq 0
        set selection (zoxide query -l | fzf --no-sort | sed 's/^[0-9,.]* *//')
        if test -n "$selection"
            cd "$selection"
        end
    else
        # Otherwise, the user is trying to jump to a location by name. Just let zoxide choose
        # the most probable match
        cd (zoxide query $argv)
    end
end

alias z=fzf_jump_cd
alias j=fzf_jump_cd

##############################################################################
# Visually navigate through the file system using yazi and put the cwd there # 
##############################################################################
function yy
    set tmp (mktemp -t "yazi-cwd.XXXXXX")
    yazi $argv --cwd-file="$tmp"
    if test -f "$tmp"
        set cwd (cat "$tmp")
        if test -n "$cwd" -a "$cwd" != "$PWD"
            cd "$cwd"
        end
        rm -f "$tmp"
    end
end
