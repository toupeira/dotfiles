# yet another dotfiles :neckbeard:

Features a [wrapper script](https://github.com/toupeira/dotfiles/blob/master/bin/dotfiles) that allows you to:

- Manage symlinks in your home directory or `/etc`, automatically using `sudo` when necessary
- Commit, pull, push, and check out sub-modules with one command
- Run Git commands in your dotfiles repository without having to `cd` there
- Update your Git submodules in parallel and get a summary of the changes
- Install Debian packages from a list stored in your repository

My configuration currently includes:

- an over-long [vimrc](https://github.com/toupeira/dotfiles/blob/master/vim/vimrc) and definitely too many [bundles](https://github.com/toupeira/dotfiles/blob/master/vim/bundle)
- a pretty fancy [tmux](https://github.com/toupeira/dotfiles/blob/master/tmux.conf) setup using some of [@bruno-'s awesome plugins](https://github.com/tmux-plugins)
- a very elaborate [bash](https://github.com/toupeira/dotfiles/blob/master/etc/profile.d) setup with lots of aliases and helper functions (which is also the reason I still haven't switched to zsh...)
- fuzzy completion in the shell using [fzf](https://github.com/junegunn/fzf) (which I keep forgetting to actually use)
- [rbenv](https://github.com/sstephenson/rbenv) and [nenv](https://github.com/ryuone/nenv) are bundled as submodules and automatically activated for bash

I use it mainly on Linux these days, but in the past I've tweaked it also for OS X and Windows.

## Adopt now

1. Initialize an empty repository
2. Add [`bin/dotfiles`](https://raw.githubusercontent.com/toupeira/dotfiles/master/bin/dotfiles)
3. Symlink it to somewhere in your `$PATH` and add your own files using `dotfiles add`
4. Run `dotfiles sync` to commit and push your changes
5. Clone your repository and run `dotfiles init` on all your other machines
6. Adapt the script to your needs, drop me a line if you add interesting features or run into bugs

## License

Unless indicated otherwise, all files in this repository (excluding submodules) are released under the [MIT License](http://opensource.org/licenses/MIT).

<sub><sup>Copyright (c) 2013 Markus Koller</sup></sub>
