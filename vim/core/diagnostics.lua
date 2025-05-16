local util = require('util')
local nmap = util.nmap

local severity = vim.diagnostic.severity

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

local with_virtual_lines = {
  virtual_lines = {
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
  },
}

vim.diagnostic.show_current_line = function()
  if vim.diagnostic.config().virtual_lines then
    return
  end

  vim.diagnostic.show(nil, 0, nil, with_virtual_lines)

  local current_line = vim.fn.line('.')
  vim.diagnostic.show_current_line_id = util.autocmd('CursorMoved', function()
    if vim.fn.line('.') ~= current_line then
      vim.diagnostic.hide_current_line()
    end
  end)
end

vim.diagnostic.hide_current_line = function()
  vim.diagnostic.config({ virtual_lines = false })

  if vim.diagnostic.show_current_line_id then
    vim.api.nvim_del_autocmd(vim.diagnostic.show_current_line_id)
    vim.diagnostic.show_current_line_id = nil
  end
end

nmap('<Leader>d', function()
  if vim.diagnostic.show_current_line_id then
    vim.diagnostic.hide_current_line()
  else
    vim.diagnostic.show_current_line()
  end
end, 'Show inline diagnostics for current line')

nmap('<LocalLeader>d', function()
  local config = vim.diagnostic.config()

  -- always disable first to clear the autocmd
  vim.diagnostic.hide_current_line()

  if not config.virtual_lines then
    vim.diagnostic.config(with_virtual_lines)
  end

  util.notify_toggle('Inline diagnostics:', not config.virtual_lines)
end, 'Toggle inline diagnostics')

nmap('<Leader>D', function() vim.diagnostic.setloclist({
  open = true,
}) end, 'Toggle diagnostics list')
