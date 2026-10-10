local util = require('util')

local servers = {
  bashls   = {},
  gdscript = { enable = true },
  lua_ls   = {},
  ruby_lsp = {},
  tailwindcss = {},
}

-- Toggle LSP servers for the current buffer's filetype
local toggle_lsp = function()
  if #util.lsp_clients() > 0 then
    util.notify_toggle(vim.bo.filetype .. ' LSP', false)
    vim.cmd.lsp('disable')
  else
    util.notify_toggle(vim.bo.filetype .. ' LSP', true)
    -- Only enable LSP servers that are explicitly configured here.
    -- `:lsp enable` will also use any servers from `nvim-lspconfig` matching the current filetype.
    for server, _ in pairs(servers) do
      local config = vim.lsp.config[server]
      if config and vim.list_contains(config.filetypes, vim.bo.filetype) then
        vim.lsp.enable(server)
      end
    end
  end
end

return {
  'neovim/nvim-lspconfig',
  event = 'LazyFile',

  dependencies = {
    { 'folke/lazydev.nvim',
      ft = 'lua',
      config = true,
    },
  },

  keys = {
    { '<Leader>%', toggle_lsp, desc = 'Toggle LSP' },
    { '<Leader>$', '<Cmd>checkhealth lsp<CR>', desc = 'Show LSP status' },
  },

  config = function()
    for server, settings in pairs(servers) do
      vim.lsp.config(server, settings.opts or {})

      if settings.enable then
        vim.lsp.enable(server)
      end
    end
  end,
}
