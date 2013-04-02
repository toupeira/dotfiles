# Installation

Clone this repository into your home folder:

    git clone https://github.com/toupeira/dotfiles.git ~/.dotfiles

Run the `dotfiles` wrapper script to interactively create symlinks for all paths:

    ~/.dotfiles/bin/dotfiles init

# Usage

    Usage: dotfiles [options] COMMAND [args]

    Options:
      -n, --dry-run      Only show changes, without performing them
      -v, --verbose      Increase verbosity
      -q, --quiet        Suppress all normal output
      -f, --force        Assume yes for all questions

    Commands:
      init               Install symlinks for all dotfiles
      install PATH...    Install symlink for PATH
      add PATH...        Add file to repository and replace it with a symlink
      ca, commitall      Commit all changes
      up, update         Update submodules (fetch new commits)
      sync               Update, commit and push

    Other arguments:
      PATH...            Edit the specified files inside the repository
      COMMAND...         Excecute a Git command inside the repository directory
