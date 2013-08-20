# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

export LANG="de_CH.UTF-8"
export LC_MESSAGES="en_GB.UTF-8"
export LC_NUMERIC="en_GB.UTF-8"
export LC_CTYPE="en_GB.UTF-8"

export RAILS_ENV=development

if [ -d ~/.profile.d ]; then
  for i in ~/.profile.d/*.sh; do
    if [ -r $i ]; then
      . $i
    fi
  done
  unset i
fi

[ -n "$BASH_INTERACTIVE" ] && login_message
