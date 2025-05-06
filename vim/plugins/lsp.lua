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
    { '<Leader>%', '<Cmd>LspInfo<CR>', desc = 'Show LSP status' },
    { '<Leader>d%', '<Cmd>LspStart<CR>', desc = 'Start LSP server' },
  },

  opts = {
    ui = {
      border = 'rounded',
    },
  },

  config = function(_, opts)
    require('lspconfig.ui.windows').default_options = opts.ui

    for server, settings in pairs(servers) do
      settings.autostart = false
      settings.install = nil
      require('lspconfig')[server].setup(settings)

      -- TODO: use `vim.lsp.config()`, it doesn't support autostart yet
      -- vim.lsp.config(server, settings)
      -- vim.lsp.enable(server)
    end

    util.autocmd('LspDetach', function(event)
      nmap('<Leader>d%', ':LspStart', { force = true, buffer = event.buf }, 'Start LSP server')
    end)

    util.autocmd('LspAttach', function(event)
      -- Enable omni-completion
      vim.bo[event.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

      -- Buffer local mappings
      local args = { force = true, buffer = event.buf }

      nmap('<Leader>d%', ':LspStop', args, 'Stop LSP server')

      nmap('gd', vim.lsp.buf.definition, args, 'Go to LSP definition')
      nmap('gD', vim.lsp.buf.declaration, args, 'Go to LSP declaration')
      nmap('gi', vim.lsp.buf.implementation, args, 'Go to LSP implementation')
      nmap('gr', vim.lsp.buf.references, args, 'Go to LSP references')
      nmap('gt', vim.lsp.buf.type_definition, args, 'Go to LSP type definition')

      nmap('<Leader>dR', vim.lsp.buf.rename, args, 'Rename LSP symbol')
      nvomap('<Leader>da', vim.lsp.buf.code_action, args, 'Run LSP code action')
      nmap('<Leader>dF', function()
        vim.lsp.buf.format { async = true }
      end, args, 'Format current file')

      nmap('<Leader>dwa', vim.lsp.buf.add_workspace_folder, args, 'Add LSP workspace folder')
      nmap('<Leader>dwr', vim.lsp.buf.remove_workspace_folder, args, 'Remove LSP workspace folder')
      nmap('<Leader>dwl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, args, 'Show LSP workspace folders')
    end)
  end
}
