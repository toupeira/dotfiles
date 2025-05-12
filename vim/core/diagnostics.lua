local util = require('util')
local nmap = util.nmap

local severity = vim.diagnostic.severity

local virtual_lines = {
  current_line = true,
  format = function(diagnostic)
    return '● ' .. diagnostic.message, ({
      [severity.ERROR] = 'DiagnosticError',
      [severity.WARN]  = 'DiagnosticWarn',
      [severity.INFO]  = 'DiagnosticInfo',
      [severity.HINT]  = 'DiagnosticHint',
      [0]              = 'DiagnosticOk',
    })[diagnostic.severity]
  end
}

nmap('<Leader>e', function()
  local config = vim.diagnostic.config()
  if config.virtual_lines then
    vim.diagnostic.config({ virtual_lines = false })
  else
    vim.diagnostic.config({ virtual_lines = virtual_lines })
  end
end, 'Toggle inline diagnostics')

nmap('<Leader>E', function()
  vim.diagnostic.setloclist({ open = true })
end, 'Toggle diagnostics list')

vim.diagnostic.config({
  severity_sort = true,
  float = false,
  underline = { severity = severity.ERROR },
  virtual_text = false,

  signs = {
    text = {
      [severity.ERROR] = '●',
      [severity.WARN]  = '●',
      [severity.INFO]  = '●',
      [severity.HINT]  = '●',
      [0]              = '●',
    },
  },
})
