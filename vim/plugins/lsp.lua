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
    { '<Leader>%', '<Cmd>checkhealth lsp<CR>', desc = 'Show LSP status' },
  },

  config = function(_, opts)
    require('lspconfig.ui.windows').default_options = opts.ui

    for server, settings in pairs(servers) do
      settings.autostart = settings.autostart or false
      settings.install = nil
      require('lspconfig')[server].setup(settings)

      -- TODO: switch to native configuration
      -- vim.lsp.config(server, settings)
    end

    -- TODO: `LspStart` and `LspStop` now require an argument and don't target all matching servers anymore
    local function lsp_start()
      local configs = require('lspconfig.util').get_config_by_ft(vim.bo.filetype)

      for _, config in ipairs(configs) do
        vim.lsp.enable(config.name)
      end
    end

    local function lsp_stop()
      local clients = vim.lsp.get_clients({ bufnr = vim.api.nvim_get_current_buf() })

      for _, client in ipairs(clients) do
        client.stop()
      end
    end

    nmap('<Leader>LS', lsp_start, 'Start LSP server')

    util.autocmd('LspDetach', function(event)
      util.unmap({ 'n' }, '<Leader>LS', { buffer = event.buf })
    end)

    util.autocmd('LspAttach', function(event)
      -- Buffer local mappings
      local args = { force = true, buffer = event.buf }

      nmap('<Leader>LS', lsp_stop, args, 'Stop LSP server')

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
