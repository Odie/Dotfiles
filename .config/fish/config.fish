# Setup homebrew vars ane enable starship
eval (/opt/homebrew/bin/brew shellenv)
starship init fish | source

######################################################################
# Basic vars                                                         #
######################################################################
set -g fish_greeting '' # Disable the default greeting message

set -gx VISUAL nvim
set -gx EDITOR "$VISUAL -f"
set -gx MANPAGER "nvim +Man!"

set -gx FZF_DEFAULT_COMMAND "rg --smart-case --files --hidden --follow --glob '!.git'"

######################################################################
# Set paths                                                          #
######################################################################

set -gx PATH $HOME/.local/bin $HOME/bin $PATH

set -gx PATH /opt/homebrew/Caskroom/miniconda/base/bin $PATH

set -gx PATH $PATH /Users/odie/.cache/lm-studio/bin

set -gx PATH "/Applications/Sublime Text.app/Contents/SharedSupport/bin/" $PATH

# Added by Docker Desktop
source /Users/odie/.docker/init-fish.sh || true

# Added by LM Studio CLI (lms)
set -gx PATH $PATH /Users/odie/.cache/lm-studio/bin

# Use fzf if installed
if type -q fzf
    function fish_user_key_bindings
        fzf_configure_bindings
    end
end

######################################################################
# Source other config files                                          #
######################################################################

# Sources a file only if it exists
function source_if_exists
    if test -f $argv[1]
        source $argv[1]
    end
end

source_if_exists ~/.config/fish/functions.fish
source_if_exists ~/.config/fish/aliases.fish
source_if_exists ~/.secrets.fish.rc

######################################################################
# Other tool init                                                    #
######################################################################
# Initialize Atuin
atuin init fish | source

zoxide init --cmd cd fish | source

# Initialize ngrok
if type -q ngrok
    eval (ngrok completion)
end
