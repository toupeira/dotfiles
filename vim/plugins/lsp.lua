local util = require('util')
local nmap = util.nmap
local nvomap = util.nvomap

local servers = {
  bashls   = { install = true },
  gdscript = { enable = true },
  lua_ls   = { install = true },
  ruby_lsp = {},
}

local install_servers = (util.is_sudo or util.is_ssh) and {} or vim.tbl_filter(
  function(key) return servers[key].install end,
  vim.tbl_keys(servers)
)

-- Only enable LSP servers that are explicitly configured here.
-- `:lsp enable` will also use any servers from `nvim-lspconfig` matching the current filetype.
local lsp_enable = function()
  for server, _ in pairs(servers) do
    local config = vim.lsp.config[server]
    if config and vim.list_contains(config.filetypes, vim.bo.filetype) then
      vim.lsp.enable(server)
    end
  end
end

return {
  'neovim/nvim-lspconfig',
  event = 'LazyFile',

  dependencies = {
    { 'mason-org/mason-lspconfig.nvim',
      dependencies = { 'mason.nvim' },
      opts = {
        ensure_installed = install_servers,
        automatic_enable = false,
      },
    },
    { 'folke/lazydev.nvim',
      ft = 'lua',
      config = true,
    },
  },

  keys = {
    { '<Leader>LS', lsp_enable, desc = 'Enable LSP' },
    { '<Leader>$', '<Cmd>checkhealth lsp<CR>', desc = 'Show LSP status' },
  },

  config = function()
    for server, settings in pairs(servers) do
      vim.lsp.config(server, settings.opts or {})

      if settings.enable then
        vim.lsp.enable(server)
      end
    end

    util.autocmd('LspDetach', function(event)
      pcall(util.unmap, { 'n' }, '<Leader>LS', { buffer = event.buf })
    end)

    util.autocmd('LspAttach', function(event)
      -- Buffer local mappings
      local args = { force = true, buffer = event.buf }

      nmap('<Leader>LS', '<Cmd>lsp disable', args, 'Disable LSP')

      nmap('gd', vim.lsp.buf.definition, args, 'Go to LSP definition')
      nmap('gD', vim.lsp.buf.declaration, args, 'Go to LSP declaration')
      nmap('gR', vim.lsp.buf.rename, args, 'Rename LSP symbol')

      nvomap('<Leader>LA', vim.lsp.buf.code_action, args, 'Run LSP code action')
      nmap('<Leader>LF', function() vim.lsp.buf.format { async = true } end, args, 'Format current file')

      nmap('<Leader>LWa', vim.lsp.buf.add_workspace_folder, args, 'Add LSP workspace folder')
      nmap('<Leader>LWr', vim.lsp.buf.remove_workspace_folder, args, 'Remove LSP workspace folder')
      nmap('<Leader>LWl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, args, 'Show LSP workspace folders')
    end)
  end,
}
