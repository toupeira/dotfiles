local util = require('util')
local nmap = util.nmap

nmap('<Leader>e', function()
  vim.diagnostic.setloclist({ open = false })
  util.toggle_list('l')
end, 'Toggle diagnostics list')

nmap('<Leader>E', vim.diagnostic.open_float, 'Toggle diagnostics popup')

local severity = {
  [vim.diagnostic.severity.ERROR] = 'Error',
  [vim.diagnostic.severity.WARN]  = 'Warn',
  [vim.diagnostic.severity.INFO]  = 'Info',
  [vim.diagnostic.severity.HINT]  = 'Hint',
  [0]                   = 'Ok',
}

local floating_highlights = {}
local sign_highlights = {}

for id, name in ipairs(severity) do
  floating_highlights[id] = 'DiagnosticFloating' .. name

  table.insert(sign_highlights, {
    name   = 'DiagnosticSign' .. name,
    text   = '●',
    texthl = 'DiagnosticSign' .. name,
    culhl  = 'DiagnosticSignCursor' .. name,
  })
end

vim.diagnostic.config({
  severity_sort = true,
  virtual_text = false,
  float = {
    prefix = function(diagnostic, _, total)
      if total == 1 then return '' end
      return '● ', floating_highlights[diagnostic.severity]
    end
  },
})

vim.fn.sign_define(sign_highlights)
