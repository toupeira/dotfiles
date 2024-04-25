local util = require('util')

return {
  'lewis6991/gitsigns.nvim',
  event = 'LazyFile',

  opts = {
    current_line_blame_opts = {
      delay = 500,
    },

    on_attach = function(bufnr)
      local gitsigns = require('gitsigns')
      local args = { buffer = bufnr, force = true }

      util.nmap(']d', function()
        if vim.wo.diff then
          vim.cmd.normal({']c', bang = true})
        else
          gitsigns.nav_hunk('next')
        end
      end, args, 'Go to next hunk')

      util.nmap('[d', function()
        if vim.wo.diff then
          vim.cmd.normal({'[c', bang = true})
        else
          gitsigns.nav_hunk('prev')
        end
      end, args, 'Go to previous hunk')

      util.nmap('<Leader>gS', gitsigns.stage_hunk, args, 'Stage current hunk')
      util.nmap('<Leader>gR', gitsigns.reset_hunk, args, 'Reset current hunk')
      util.nmap('<Leader>gU', gitsigns.undo_stage_hunk, args, 'Undo last staged hunk')

      util.vmap('<Leader>gs', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Stage selected hunk')
      util.vmap('<Leader>gr', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end, args, 'Reset selected hunk')

      util.nmap('<Leader>gB', gitsigns.toggle_current_line_blame, args, 'Toggle blame for current line')

      util.map({'o', 'x'}, 'ih', ':Gitsigns select_hunk', args)
    end
  }
}
