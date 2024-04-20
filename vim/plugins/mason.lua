return {
  'williamboman/mason.nvim',
  event = 'VeryLazy',
  build = ':MasonUpdate',

  keys = {
    { '<leader>@', '<Cmd>Mason<CR>', desc = 'Open Mason' }
  },

  opts = {
    ensure_installed = {
      'jsonlint',
      'luacheck',
      'stylua',
      'shfmt',
      'shellcheck',
      'stylelint',
      'yamllint',
    },

    install_root_dir = '/etc/dotfiles/packages/mason',

    ui = {
      check_outdated_packages_on_open = false,
      border = 'rounded',
      width = 0.9,
      height = 0.8,
    },
  },

  config = function(_, opts)
    require('mason').setup(opts)
    local mr = require('mason-registry')
    mr:on('package:install:success', function()
      vim.defer_fn(function()
        require('lazy.core.handler.event').trigger({
          event = 'FileType',
          buf = vim.api.nvim_get_current_buf(),
        })
      end, 100)
    end)

    local function ensure_installed()
      for _, tool in ipairs(opts.ensure_installed) do
        local p = mr.get_package(tool)
        if not p:is_installed() then
          p:install()
        end
      end
    end

    if mr.refresh then
      mr.refresh(ensure_installed)
    else
      ensure_installed()
    end
  end
}
