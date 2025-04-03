local util = require('util')
local nmap = util.nmap

local last_win
nmap('<Leader>e', function()
  if last_win and vim.api.nvim_win_is_valid(last_win) then
    vim.api.nvim_win_close(last_win, false)
    last_win = nil
  else
    last_win = select(2, vim.diagnostic.open_float())
  end
end, 'Toggle diagnostics popup')

nmap('<Leader>E', function()
  vim.diagnostic.setloclist({ open = false })
  util.toggle_list('l')
end, 'Toggle diagnostics list')

vim.diagnostic.config({
  severity_sort = true,
  virtual_text = false,
  float = {
    border = 'rounded',
    prefix = function(diagnostic, _, total)
      if total == 1 then return '' end
      return '● ', ({
        [vim.diagnostic.severity.ERROR] = 'DiagnosticFloatingError',
        [vim.diagnostic.severity.WARN]  = 'DiagnosticFloatingWarn',
        [vim.diagnostic.severity.INFO]  = 'DiagnosticFloatingInfo',
        [vim.diagnostic.severity.HINT]  = 'DiagnosticFloatingHint',
        [0]                             = 'DiagnosticFloatingOk',
      })[diagnostic.severity]
    end
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '●',
      [vim.diagnostic.severity.WARN]  = '●',
      [vim.diagnostic.severity.INFO]  = '●',
      [vim.diagnostic.severity.HINT]  = '●',
      [0] = '●',
    },
  },
})
