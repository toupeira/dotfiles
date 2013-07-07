# Check for interactive bash and that we haven't already been sourced.
[ -n "$BASH_INTERACTIVE" -a -z "$BASH_COMPLETION" -a -z "$BASH_COMPLETION_COMPAT_DIR" ] || return

# Check for recent enough version of bash.
bash=${BASH_VERSION%.*}; bmajor=${bash%.*}; bminor=${bash#*.}
if [ $bmajor -gt 4 ] || [ $bmajor -eq 4 -a $bminor -ge 1 ]; then
    [ -r "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion" ] && \
        . "${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion"
    if shopt -q progcomp && [ -r /etc/bash_completion ]; then
        # Source Git helpers
        . /usr/share/bash-completion/completions/git

        # Source completion code.
        . /etc/bash_completion
    fi
fi
unset bash bmajor bminor
