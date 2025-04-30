local util = require('util')
local map = util.map
local nmap = util.nmap

return {
  'milanglacier/yarepl.nvim',
  cmd = {
    'AiderExec',
    'AiderSendDrop',
    'AiderSendLs',
    'AiderSendNo',
    'AiderSendReset',
    'AiderSendYes',
    'AiderSetPrefix',
    'REPLClose',
    'REPLExec',
    'REPLFocus',
    'REPLHide',
    'REPLHideOrFocus',
    'REPLSendLine',
    'REPLSendOperator',
    'REPLSendVisual',
    'REPLSourceOperator',
    'REPLSourceVisual',
    'REPLStart',
    'REPLSwap',
  },

  opts = function()
    local wincmd = function(bufnr, _name)
      local split, width, height
      width = 1
      height = 1
      if vim.o.columns >= 100 then
        split = 'right'
        width = 0.35
      else
        split = 'below'
        height = 0.4
      end

      vim.api.nvim_open_win(bufnr, true, {
        split = split,
        width = math.floor(vim.o.columns * width),
        height = math.floor(vim.o.lines * height),
      })
    end

    return {
      buflisted = false,
      wincmd = wincmd,
      metas = {
        aider = util.merge(require('yarepl.extensions.aider').create_aider_meta(), {
          wincmd = wincmd,
        }),
        ruby = { cmd = 'pry', formatter = 'bracketed_pasting' },

        aichat = false,
        radian = false,
        python = false,
        R = false,
        zsh = false,
      },
    }
  end,

  init = function()
    local toggle = function(name)
      local repl = require('yarepl')._get_repl(nil, name, vim.api.nvim_get_current_buf())
      local buffer = vim.fn.expand('%:p')

      if repl then
        vim.cmd.REPLHideOrFocus(name)
        if vim.bo.filetype == 'REPL' then
          vim.cmd.startinsert()
        end
      else
        vim.cmd.REPLStart(name)
      end

      if name == 'aider' and vim.fn.filereadable(buffer) == 1 then
        vim.cmd.AiderExec('/add ' .. buffer)
      end
    end

    util.autocmd({ 'BufWinEnter', 'WinEnter' }, '#*#*', 'startinsert!')

    map({ 'n', 't' }, '<M-z>', function() toggle('bash') end, { desc = 'Toggle shell terminal' })
    map({ 'n', 't' }, '<M-a>', function() toggle('aider') end, { desc = 'Aider: Toggle terminal' })
    nmap('<Leader>!', '<M-z>', { remap = true })

    nmap('<Leader>aa', function() vim.cmd.AiderExec('/add', vim.fn.expand('%:p')) end, { desc = 'Aider: Add current file' })
    nmap('<Leader>ad', function() vim.cmd.AiderExec('/drop', vim.fn.expand('%:p')) end, { desc = 'Aider: Drop current file' })
    nmap('<Leader>al', ':AiderSendLs', { desc = 'Aider: List files' })
    nmap('<Leader>aD', ':AiderSendDrop', { desc = 'Aider: Drop all files' })

    nmap('<Leader>ay', ':AiderSendYes', { desc = 'Aider: Confirm prompt' })
    nmap('<Leader>an', ':AiderSendNo', { desc = 'Aider: Reject prompt' })
    nmap('<Leader>aR', ':AiderSendReset', { desc = 'Aider: Reset history' })

    nmap('<Leader>ap', ':AiderSetPrefix', { desc = 'Aider: Set prefix' })
  end,
}
