local util = require('util')

return {
  { 'tmux-plugins/tmux-open', lazy = true },
  { 'tmux-plugins/tmux-yank', lazy = true },

  { 'alexghergh/nvim-tmux-navigation',
    event = 'VeryLazy',
    cond = function()
      return os.getenv('TMUX')
    end,

    opts = {
      disable_when_zoomed = true,
      keybindings = {
        left = '<C-h>',
        down = '<C-j>',
        up = '<C-k>',
        right = '<C-l>',
        modes = { 'normal', 'command', 'visual', 'terminal' },
      }
    },

    config = function(_, opts)
      local tmux_nav = require('nvim-tmux-navigation')
      tmux_nav.setup(opts)

      -- don't navigate to the tmux pane on the other side
      -- of the window when reaching the edge
      local tmux_util = require'nvim-tmux-navigation.tmux_util'
      local tmux_change_pane = tmux_util.tmux_change_pane

      tmux_util.tmux_change_pane = function(direction)
        local win = vim.fn.winnr()
        local last_win = util.window_count()
        local pane, last_pane = unpack(
          vim.tbl_map(tonumber, util.split(
            vim.fn.system('tmux display -p "#{pane_index} #{window_panes}"')
          ))
        )

        if (direction == 'k' or direction == 'h') and win == 1 and pane == 1 then
          return
        end

        if (direction == 'j' or direction == 'l') and win == last_win and pane == last_pane then
          return
        end

        return tmux_change_pane(direction)
      end
    end
  },
}
