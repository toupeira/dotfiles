local util = require('util')

return {
  'williamboman/mason.nvim',
  build = ':MasonUpdate',
  dependencies = {
    { 'WhoIsSethDaniel/mason-tool-installer.nvim',
      event = 'VeryLazy',
      opts = {
        ensure_installed = (util.is_sudo or util.is_ssh) and {} or {
          'jsonlint',
          'luacheck',
          'shellcheck',
          'stylelint',
          'yamllint',
        },
      },
    },
  },

  keys = {
    { '<leader>#', '<Cmd>Mason<CR>', desc = 'Open Mason' }
  },

  opts = {
    install_root_dir = '/etc/dotfiles/packages/mason',

    ui = {
      check_outdated_packages_on_open = false,
      border = 'rounded',
      width = 0.9,
      height = 0.8,
    },
  }
}
