if [[ -d $HOME/.config/zsh ]]; then
    export ZDOTDIR=$HOME/.config/zsh
else
    echo "$HOME/.config/zsh not found"
fi

export PATH=$HOME/bin:$HOME/.cask/bin:/opt/homebrew/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/odie/.tools/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/odie/.tools/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/odie/.tools/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/odie/.tools/google-cloud-sdk/completion.zsh.inc'; fi

. "$HOME/.cargo/env"
