local util = require('util')

return {
  'stevearc/aerial.nvim',
  cmd = {
    'AerialToggle',
    'AerialNavToggle',
    'AerialNext',
    'AerialPrev',
    'AerialGo',
  },

  init = function()
    util.nmap('<Leader>t', ':AerialNavToggle', 'Toggle symbols in popup')
    util.nmap('<Leader>T', ':AerialToggle', 'Toggle symbols in sidebar')

    util.nmap(']]', ':AerialNext', 'Go to next symbol')
    util.nmap('[[', ':AerialPrev', 'Go to previous symbol')
  end,

  opts = {
    attach_mode = 'global',
    post_jump_cmd = "normal! zvzz",

    layout = {
      min_width = 12,
      max_width = { 40, 0.3 },
      default_direction = 'right',
      placement = 'edge',
    },

    nav = {
      min_width = 0.29,
      max_width = 0.29,
      min_height = 0.6,
      max_height = 0.9,
      win_opts = { winblend = 0 },
      keymaps = { q = 'actions.close' },
      preview = true,
    },
  },

  config = function(_, opts)
    require('aerial').setup(opts)

    local config = require('aerial.config')
    local setup = config.setup
    config.setup = function(pending_opts)
      setup(pending_opts)

      -- remove spaces after icons
      -- https://github.com/stevearc/aerial.nvim/pull/360
      for type, icon in pairs(config.default_icons) do
        config.default_icons[type] = icon:gsub(' ', '')
      end

      -- replace some icons
      config.default_icons.Interface = ''
    end
  end
}
