local util = require('util')
local nmap = util.nmap

local diag = vim.diagnostic

nmap('<Leader>e', diag.open_float, 'Toggle diagnostics popup')
nmap('<Leader>E', function()
  diag.setloclist({ open = false })
  util.toggle_list('l')
end, 'Toggle diagnostics list')

local severity = {
  [diag.severity.ERROR] = 'Error',
  [diag.severity.WARN]  = 'Warn',
  [diag.severity.INFO]  = 'Info',
  [diag.severity.HINT]  = 'Hint',
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

diag.config({
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
