# Installation

Clone this repository into your home folder:

    git clone https://github.com/toupeira/dotfiles.git ~/.dotfiles

Run the `dotfiles` wrapper script to interactively create symlinks for all paths:

    ~/.dotfiles/bin/dotfiles init

# Usage

    Usage: dotfiles [options] COMMAND [args]

    Options:
      -n, --dry-run   Only show changes, without performing them
      -v, --verbose   Increase verbosity
      -q, --quiet     Suppress all normal output
      -f, --force     Assume yes for all questions
          --path      Show path to the dotfiles repository and exit

    Commands:
      init                  Install symlinks for all dotfiles
      install PATH...       Install symlink for PATH
      add PATH...           Add file to repository and replace it with a symlink
      ca, commitall         Commit all changes
      up, update [MODULE]   Update submodules (fetch new commits)
      sync                  Update, commit and push

    Other arguments:
      COMMAND [ARGS]...     Excecute a Git command inside the dotfiles repository


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/toupeira/dotfiles/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

