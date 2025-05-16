local util = require('util')
local nmap = util.nmap
local vmap = util.vmap

return {
  'lewis6991/gitsigns.nvim',
  event = 'VeryLazy',

  opts = {
    sign_priority = 100,

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

      local next_hunk, prev_hunk = util.make_repeatable(
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

      nmap(']c', next_hunk, args, 'Jump to next hunk')
      nmap('[c', prev_hunk, args, 'Jump to previous hunk')

      nmap('<Leader>ga', gitsigns.stage_hunk, args, 'Stage current hunk')
      nmap('<Leader>gR', gitsigns.reset_hunk, args, 'Reset current hunk')
      nmap('<Leader>gu', gitsigns.undo_stage_hunk, args, 'Undo last staged hunk')

      vmap('<Leader>ga', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Stage selected hunk')
      vmap('<Leader>gR', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Reset selected hunk')

      nmap('<Leader>gb', gitsigns.blame, args, 'Open Git blame for current file')
      nmap('<Leader>gB', gitsigns.toggle_current_line_blame, args, 'Toggle blame for current line')

      util.map({'o', 'x'}, 'ih', ':Gitsigns select_hunk', args)
    end
  }
}
