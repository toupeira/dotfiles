local util = require('util')

return {
  'hrsh7th/nvim-cmp',
  event = 'VeryLazy',
  dependencies = {
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-emoji',
    'hrsh7th/cmp-calc',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'hrsh7th/cmp-path',

    { 'andersevenrud/cmp-tmux',
      cond = function()
        return os.getenv('TMUX')
      end
    },
  },

  opts = {
    completion = {
      completeopt = vim.o.completeopt,
      keyword_length = 2,
    },

    sources = {
      { name = 'nvim_lsp' },
      { name = 'nvim_lsp_signature_help' },
      { name = 'buffer' },
      { name = 'tmux' },
      { name = 'calc' },
      { name = 'emoji' },
    },
  },

  config = function(_, opts)
    local cmp = require('cmp')

    opts.mapping = cmp.mapping.preset.insert({
      ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.confirm({ select = true })
        else
          fallback()
        end
      end, { 'i', 's', 'c' }),
    })

    cmp.setup(opts)

    cmp.setup.filetype('lua', {
      sources = util.merge(opts.sources, {{ name = 'nvim_lua' }})
    })

    cmp.setup.cmdline({ '/', '?' }, {
      mapping = util.merge(cmp.mapping.preset.cmdline(), opts.mapping),
    })

    cmp.setup.cmdline(':', {
      mapping = util.merge(cmp.mapping.preset.cmdline(), opts.mapping),
      sources = cmp.config.sources({{ name = 'path' }}, {{ name = 'cmdline' }}),
      matching = { disallow_symbol_nonprefix_matching = false }
    })
  end
}
