local util = require('util')

return {
  { 'tmux-plugins/tmux-open', lazy = true },
  { 'tmux-plugins/tmux-yank', lazy = true },

  { 'smart-splits-nvim/smart-splits.nvim',
    event = 'VeryLazy',
    cond = function()
      return (util.is_tmux and not util.is_headless)
    end,
    dependencies = {
      { 'smart-splits-nvim/backend-tmux', main = 'smart-splits-backend-tmux' },
    },

    opts = {
      move = { at_edge = 'stop' },
      mux = { backend = 'smart-splits-backend-tmux' },
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
