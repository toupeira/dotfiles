local util = require('util')
local nmap = util.nmap
local vmap = util.vmap

return {
  'lewis6991/gitsigns.nvim',
  event = 'VeryLazy',

  opts = {
    attach_to_untracked = true,
    sign_priority = 100,

    signs = {
      add          = { text = '▉' },
      change       = { text = '▉' },
      untracked    = { text = '▉' },
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
      local config = require('gitsigns.config').config
      local args = { buffer = bufnr, force = true }

      -- luacheck: globals MiniDiff
      local next_hunk, prev_hunk = util.make_repeatable(
        function()
          local mini_diff_data = MiniDiff.get_buf_data()
          if mini_diff_data and #mini_diff_data.hunks > 0 then
            MiniDiff.goto_hunk('next')
          elseif vim.wo.diff then
            vim.cmd.normal({ ']c', bang = true })
          else
            gitsigns.nav_hunk('next', { target = 'all' })
          end
        end,

        function()
          local mini_diff_data = MiniDiff.get_buf_data()
          if mini_diff_data and #mini_diff_data.hunks > 0 then
            MiniDiff.goto_hunk('prev')
          elseif vim.wo.diff then
            vim.cmd.normal({ '[c', bang = true })
          else
            gitsigns.nav_hunk('prev', { target = 'all' })
          end
        end
      )

      local next_unstaged_hunk, prev_unstaged_hunk = util.make_repeatable(
        function()
          gitsigns.nav_hunk('next', { target = 'unstaged' })
        end,
        function()
          gitsigns.nav_hunk('prev', { target = 'unstaged' })
        end
      )

      nmap(']c', next_hunk, args, 'Jump to next hunk')
      nmap('[c', prev_hunk, args, 'Jump to previous hunk')
      nmap(']C', next_unstaged_hunk, args, 'Jump to next unstaged hunk')
      nmap('[C', prev_unstaged_hunk, args, 'Jump to previous unstaged hunk')

      nmap('<Leader>ga', gitsigns.stage_hunk, args, 'Stage current hunk')
      nmap('<Leader>gR', gitsigns.reset_hunk, args, 'Reset current hunk')
      nmap('<Leader>gu', gitsigns.undo_stage_hunk, args, 'Undo last staged hunk')

      vmap('<Leader>ga', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Stage selected hunk')
      vmap('<Leader>gR', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Reset selected hunk')

      nmap('<LocalLeader>gd', function()
        gitsigns.toggle_numhl()
        gitsigns.toggle_linehl()
        gitsigns.toggle_word_diff()

        util.notify_toggle('Inline Diff:', config.linehl)
      end, args, 'Toggle inline diff')

      nmap('<Leader>gb', gitsigns.blame, args, 'Open Git blame for current file')
      nmap('<LocalLeader>gb', function()
        gitsigns.toggle_current_line_blame()

        util.notify_toggle('Inline Blame:', config.current_line_blame)
      end, args, 'Toggle blame for current line')

      util.map({'o', 'x'}, 'ih', ':Gitsigns select_hunk', args)
    end,
  },
}
