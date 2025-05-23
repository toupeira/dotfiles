-- From https://codecompanion.olimorris.dev/usage/ui.html#lualine-nvim-integration

local M = require('lualine.component'):extend()

M.processing = false
M.request = {}
M.spinner_index = 1

local spinner_symbols = { '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏' }

function M:init(options)
  M.super.init(self, options)

  local group = vim.api.nvim_create_augroup('CodeCompanionHooks', {})

  vim.api.nvim_create_autocmd({ 'User' }, {
    pattern = 'CodeCompanionRequest*',
    group = group,
    callback = function(request)
      if request.match == 'CodeCompanionRequestStarted' then
        self.processing = true
        self.request = {
          strategy = request.data.strategy,
          adapter = request.data.adapter.name,
          model = request.data.adapter.model,
        }
      elseif request.match == 'CodeCompanionRequestFinished' then
        self.processing = false
      end
    end,
  })
end

function M:update_status()
  if self.processing then
    self.spinner_index = (self.spinner_index % #spinner_symbols) + 1
    return spinner_symbols[self.spinner_index] .. ' ' .. (self.request.strategy or self.request.adapter) .. ' ⚗️'
  end
end

return M
