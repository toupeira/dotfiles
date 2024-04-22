local util = require('util')
local map = util.map
local nmap = util.nmap

return {
  'neovim/nvim-lspconfig',
  event = 'LazyFile',
  dependencies = {
    { 'williamboman/mason.nvim' },
    { 'williamboman/mason-lspconfig.nvim' },
    { 'folke/neodev.nvim', config = true },
  },

  keys = {
    { '<Leader>#', '<Cmd>LspInfo<CR>', desc = 'Show LSP status' },
    { '<Leader>d#', '<Cmd>LspStart<CR>', desc = 'Start LSP server' },
  },

  opts = {
    servers = {
      eslint = {},
      lua_ls = {},
      vimls = {},
    },

    ui = {
      border = 'rounded',
    },
  },

  config = function(_, opts)
    require('lspconfig.ui.windows').default_options = opts.ui

    util.autocmd('LspDetach', function(event)
      nmap('<Leader>d#', ':LspStart', { force = true, buffer = event.buf }, 'Start LSP server')
    end)

    util.autocmd('LspAttach', function(event)
      -- Enable omni-completion
      vim.bo[event.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

      -- Buffer local mappings
      local args = { force = true, buffer = event.buf }

      nmap('<Leader>d#', ':LspStop', args, 'Stop LSP server')

      nmap('gd', vim.lsp.buf.definition, args, 'Go to definition')
      nmap('gD', vim.lsp.buf.declaration, args, 'Go to declaration')
      nmap('gi', vim.lsp.buf.implementation, args, 'Go to implementation')
      nmap('gr', vim.lsp.buf.references, args, 'Go to references')
      nmap('gt', vim.lsp.buf.type_definition, args, 'Go to type definition')

      nmap('K', vim.lsp.buf.hover, args, 'Show context help')
      -- nmap('<C-k>', vim.lsp.buf.signature_help, args, 'Show signature help')

      nmap('<Leader>dR', vim.lsp.buf.rename, args, 'Rename symbol')
      map({ 'n', 'v' }, '<Leader>da', vim.lsp.buf.code_action, args, 'Run code action')
      nmap('<Leader>dF', function()
        vim.lsp.buf.format { async = true }
      end, args, 'Format current file')

      nmap('<Leader>dwa', vim.lsp.buf.add_workspace_folder, args, 'Add workspace folder')
      nmap('<Leader>dwr', vim.lsp.buf.remove_workspace_folder, args, 'Remove workspace folder')
      nmap('<Leader>dwl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, args, 'Show workspace folders')
    end)

    local servers = opts.servers
    local has_cmp, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
    local capabilities = vim.tbl_deep_extend(
      'force',
      {},
      vim.lsp.protocol.make_client_capabilities(),
      has_cmp and cmp_nvim_lsp.default_capabilities() or {},
      opts.capabilities or {}
    )

    local function setup(server)
      local server_opts = vim.tbl_deep_extend('force', {
        autostart = false,
        capabilities = vim.deepcopy(capabilities),
      }, servers[server] or {})

      require('lspconfig')[server].setup(server_opts)
    end

    -- get all the servers that are available through mason-lspconfig
    local have_mason, mlsp = pcall(require, 'mason-lspconfig')
    local all_mslp_servers = {}
    if have_mason then
      all_mslp_servers = vim.tbl_keys(require('mason-lspconfig.mappings.server').lspconfig_to_package)
    end

    local ensure_installed = {} ---@type string[]
    for server, server_opts in pairs(servers) do
      if server_opts then
        server_opts = server_opts == true and {} or server_opts
        -- run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
        if server_opts.mason == false or not vim.tbl_contains(all_mslp_servers, server) then
          setup(server)
        elseif server_opts.enabled ~= false then
          ensure_installed[#ensure_installed + 1] = server
        end
      end
    end

    if have_mason then
      mlsp.setup({ ensure_installed = ensure_installed, handlers = { setup } })
    end
  end
}
