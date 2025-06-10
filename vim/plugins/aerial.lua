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

  keys = {
    { '<LocalLeader>i', '<Cmd>AerialToggle<CR>', desc = 'Toggle symbols sidebar' },
  },

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
      min_width = 0.3,
      max_width = 0.3,
      min_height = 0.8,
      max_height = 0.8,
      win_opts = { winblend = 0 },
      keymaps = { q = 'actions.close' },
      preview = true,
    },
  },

  config = function(_, opts)
    local aerial = require('aerial')
    aerial.setup(opts)

    local next_symbol, previous_symbol = util.make_repeatable(aerial.next, aerial.prev)

    nmap(']]', next_symbol, { force = true }, 'Go to next symbol')
    nmap('[[', previous_symbol, { force = true }, 'Go to previous symbol')
  end,
}
