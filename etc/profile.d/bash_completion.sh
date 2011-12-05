# Check for interactive bash and that we haven't already been sourced.
[ -n "$BASH_INTERACTIVE" -a -z "$BASH_COMPLETION" ] || return

# Check for recent enough version of bash.
bash=${BASH_VERSION%.*}; bmajor=${bash%.*}; bminor=${bash#*.}
if [ $bmajor -gt 3 ] || [ $bmajor -eq 3 -a $bminor -ge 2 ]; then
    if shopt -q progcomp && [ -r /etc/bash_completion ]; then
        # Source completion code.
        . /etc/bash_completion
    fi
fi
unset bash bmajor bminor
