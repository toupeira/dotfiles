local util = require('util')

return {
  'stevearc/aerial.nvim',
  cmd = { 'AerialToggle', 'AerialNavToggle' },

  init = function()
    -- TODO: implement
    -- nmap('<Leader>t', require('util/fzf-aerial'))
    util.nmap('<Leader>t', ':AerialNavToggle', 'Toggle symbols in popup')
    util.nmap('<Leader>T', ':AerialToggle', 'Toggle symbols in sidebar')

    util.nvomap(']t', ':AerialNext', 'Go to next symbol')
    util.nmap(']T', ':9999AerialNext', 'Go to last symbol')
    util.nvomap('[t', ':AerialPrev', 'Go to previous symbol')
    util.nvomap('[T', ':AerialGo 1', 'Go to first symbol')
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
  }
}
