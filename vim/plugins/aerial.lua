local util = require('util')
local nmap = util.nmap

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
    local aerial = require('aerial')
    aerial.setup(opts)

    local repeat_move = require('nvim-treesitter.textobjects.repeatable_move')
    local next_symbol, previous_symbol = repeat_move.make_repeatable_move_pair(aerial.next, aerial.prev)

    nmap(']]', next_symbol, { force = true }, 'Go to next symbol')
    nmap('[[', previous_symbol, { force = true }, 'Go to previous symbol')

    nmap('<Leader>a', aerial.nav_toggle, 'Toggle symbols in popup')
    nmap('<Leader>A', aerial.toggle, 'Toggle symbols in sidebar')

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
