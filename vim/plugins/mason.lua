local util = require('util')

return {
  'mason-org/mason.nvim',
  event = 'VeryLazy',
  build = ':MasonUpdate',
  dependencies = { -- {{{
    { 'WhoIsSethDaniel/mason-tool-installer.nvim',
      opts = {
        ensure_installed = (util.is_sudo or util.is_ssh) and {} or {
          'jsonlint',
          'luacheck',
          'shellcheck',
          'stylua',
          'yamllint',
        },
        integrations = {
          ['mason-lspconfig'] = false,
          ['mason-null-ls'] = false,
          ['mason-nvim-dap'] = false,
        },
      },
    },
  }, -- }}}

  keys = {
    { '<leader>#', '<Cmd>Mason<CR>', desc = 'Open Mason' }
  },

  opts = {
    install_root_dir = '/slack/dotfiles/packages/mason',

    ui = {
      check_outdated_packages_on_open = false,
      border = 'rounded',
      width = 0.9,
      height = 0.9,
    },
  },
}
