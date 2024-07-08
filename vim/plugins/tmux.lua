local util = require('util')
local map = util.map

return {
  { 'tmux-plugins/tmux-open', lazy = true },
  { 'tmux-plugins/tmux-yank', lazy = true },

  { 'mrjones2014/smart-splits.nvim',
    event = 'VeryLazy',
    cond = function()
      return (util.is_tmux and not util.is_headless) or util.is_neovide
    end,

    opts = {
      at_edge = 'stop',
      float_win_behavior = 'mux',
      multiplexer_integration = 'tmux',
    },

    init = function()
      local splits = require('smart-splits')
      local modes = { 'n', 'c', 'v', 't' }

      map(modes, '<C-h>', splits.move_cursor_left, 'Go to window on the left')
      map(modes, '<C-j>', splits.move_cursor_down, 'Go to window on the bottom')
      map(modes, '<C-k>', splits.move_cursor_up, 'Go to window on the top')
      map(modes, '<C-l>', splits.move_cursor_right, 'Go to window on the right')
      map(modes, '<C-\\>', splits.move_cursor_previous, 'Go to previous window')
    end
  },
}
