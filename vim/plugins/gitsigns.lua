local util = require('util')
local nmap = util.nmap
local vmap = util.vmap

return {
  'lewis6991/gitsigns.nvim',
  event = 'VeryLazy',

  opts = {
    signs = {
      add          = { text = '▉' },
      change       = { text = '▉' },
      untracked    = { text = '○' },
      delete       = { text = '⨯' },
      topdelete    = { text = '⨯' },
    },
    signs_staged = {
      add          = { text = '▉' },
      change       = { text = '▉' },
      untracked    = { text = '▉' },
      delete       = { text = '▉' },
      topdelete    = { text = '▉' },
    },

    current_line_blame_opts = {
      delay = 500,
    },

    on_attach = function(bufnr)
      local gitsigns = require('gitsigns')
      local args = { buffer = bufnr, force = true }

      local repeat_move = require('nvim-treesitter.textobjects.repeatable_move')
      local next_hunk, prev_hunk = repeat_move.make_repeatable_move_pair(
        function()
          if vim.wo.diff then
            vim.cmd.normal({']c', bang = true})
          else
            gitsigns.nav_hunk('next')
          end
        end,

        function()
          if vim.wo.diff then
            vim.cmd.normal({'[c', bang = true})
          else
            gitsigns.nav_hunk('prev')
          end
        end
      )

      nmap(']d', next_hunk, args, 'Jump to next hunk')
      nmap('[d', prev_hunk, args, 'Jump to previous hunk')

      nmap('<Leader>ga', gitsigns.stage_hunk, args, 'Stage current hunk')
      nmap('<Leader>gR', gitsigns.reset_hunk, args, 'Reset current hunk')
      nmap('<Leader>gu', gitsigns.undo_stage_hunk, args, 'Undo last staged hunk')

      vmap('<Leader>ga', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Stage selected hunk')
      vmap('<Leader>gR', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Reset selected hunk')

      nmap('<Leader>gb', gitsigns.toggle_current_line_blame, args, 'Toggle blame for current line')

      util.map({'o', 'x'}, 'ih', ':Gitsigns select_hunk', args)
    end
  }
}
