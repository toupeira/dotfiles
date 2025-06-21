local util = require('util')
local nmap = util.nmap
local nvomap = util.nvomap

local servers = {
  bashls = { install = true },
  lua_ls = { install = true },
  ruby_lsp = {},
}

local install_servers = (util.is_sudo or util.is_ssh) and {} or vim.tbl_filter(
  function(key) return servers[key]['install'] end,
  vim.tbl_keys(servers)
)

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
    { '<Leader>LS', '<Cmd>LspStart<CR>', 'Start LSP server' },
    { '<Leader>%', '<Cmd>checkhealth lsp<CR>', desc = 'Show LSP status' },
  },

  config = function()
    for server, settings in pairs(servers) do
      settings.install = nil
      vim.lsp.config(server, settings)
    end

    util.autocmd('LspDetach', function(event)
      util.unmap({ 'n' }, '<Leader>LS', { buffer = event.buf })
    end)

    util.autocmd('LspAttach', function(event)
      -- Buffer local mappings
      local args = { force = true, buffer = event.buf }

      nmap('<Leader>LS', '<Cmd>LspStop<CR>', args, 'Stop LSP server')

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
