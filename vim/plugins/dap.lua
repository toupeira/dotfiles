local util = require('util')

return {
  'https://codeberg.org/mfussenegger/nvim-dap.git',

  dependencies = {
    'https://codeberg.org/mfussenegger/nluarepl.git',
  },

  cmd = {
    'DapClearBreakpoints',
    'DapContinue',
    'DapDisconnect',
    'DapEval',
    'DapNew',
    'DapPause',
    'DapRestartFrame',
    'DapSetLogLevel',
    'DapShowLog',
    'DapStepInto',
    'DapStepOut',
    'DapStepOver',
    'DapTerminate',
    'DapToggleBreakpoint',
    'DapToggleRepl',
  },

  keys = {
    { '<Leader>PS', '<Cmd>DapContinue<CR>', desc = 'Start or continue debug session' },
    { '<Leader>PT', '<Cmd>DapTerminate<CR>', desc = 'Terminate debug session' },

    { '<Leader>PR', function()
      local dap = require('dap')
      if not dap.session() then
        dap.continue()
      end

      dap.repl.toggle({ height = math.floor(vim.o.lines * 0.3) })
    end, desc = 'Toggle debug console' },
  },

  config = function()
    util.autocmd({ 'BufWinEnter', 'WinEnter' }, '\\[dap-repl-*', 'startinsert!')

    util.autocmd('FileType', 'dap-repl', function(event)
      local args = { buffer = event.buf, remap = true, force = true }
      util.imap('<C-h>', '<C-o><C-h>', args, 'Go to window on the left')
      util.imap('<C-j>', '<C-o><C-j>', args, 'Go to window on the bottom')
      util.imap('<C-k>', '<C-o><C-k>', args, 'Go to window on the top')
      util.imap('<C-l>', '<C-o><C-l>', args, 'Go to window on the right')
      util.nmap('q', '<Cmd>DapToggleRepl<CR>', util.merge(args, { remap = false }))

      vim.b.completion = false
      vim.wo[0][0].winhighlight = 'Normal:NormalFloat'

      require('dap.ext.autocompl').attach()

      vim.schedule(function()
        vim.cmd.wincmd('j')
      end)
    end)
  end
}
