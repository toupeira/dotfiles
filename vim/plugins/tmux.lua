local util = require('util')

return {
  { 'tmux-plugins/tmux-open', lazy = true },
  { 'tmux-plugins/tmux-yank', lazy = true },

  { 'mrjones2014/smart-splits.nvim',
    event = 'VeryLazy',
    cond = function()
      return (util.is_tmux and not util.is_headless)
    end,

    opts = {
      at_edge = 'stop',
      float_win_behavior = 'mux',
      multiplexer_integration = 'tmux',
    },

    keys = {
      { '<C-h>', mode = { 'n', 'c', 'v', 't' }, '<Cmd>lua require("smart-splits").move_cursor_left()<CR>', desc = 'Go to window on the left' },
      { '<C-j>', mode = { 'n', 'c', 'v', 't' }, '<Cmd>lua require("smart-splits").move_cursor_down()<CR>', desc = 'Go to window on the bottom' },
      { '<C-k>', mode = { 'n', 'c', 'v', 't' }, '<Cmd>lua require("smart-splits").move_cursor_up()<CR>', desc = 'Go to window on the top' },
      { '<C-l>', mode = { 'n', 'c', 'v', 't' }, '<Cmd>lua require("smart-splits").move_cursor_right()<CR>', desc = 'Go to window on the right' },
      { '<C-\\>', mode = { 'n', 'c', 'v', 't' }, '<Cmd>lua require("smart-splits").move_cursor_previous()<CR>', desc = 'Go to previous window' },
    },
  },
}
