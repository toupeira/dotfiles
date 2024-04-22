local util = require('util')

local icons = {
  kinds = {
    Array         = " ",
    Boolean       = "󰨙 ",
    Class         = "󰆧 ",
    Codeium       = "󰘦 ",
    Color         = " ",
    Control       = " ",
    Collapsed     = " ",
    Constant      = "󰏿 ",
    Constructor   = " ",
    Copilot       = " ",
    Enum          = " ",
    EnumMember    = " ",
    Event         = " ",
    Field         = " ",
    File          = " ",
    Folder        = " ",
    Function      = "󰊕 ",
    Interface     = " ",
    Key           = " ",
    Keyword       = " ",
    Method        = "󰊕 ",
    Module        = " ",
    Namespace     = "󰦮 ",
    Null          = " ",
    Number        = "󰎠 ",
    Object        = " ",
    Operator      = "󰆕 ",
    Package       = " ",
    Property      = " ",
    Reference     = " ",
    Snippet       = " ",
    String        = " ",
    Struct        = "󰆼 ",
    TabNine       = "󰏚 ",
    Text          = " ",
    TypeParameter = " ",
    Unit          = " ",
    Value         = " ",
    Variable      = "󰀫 ",
  },
}

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
end

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

    { 'L3MON4D3/LuaSnip',
      dependencies = {
        'rafamadriz/friendly-snippets',
        'saadparwaiz1/cmp_luasnip',
      },
      config = function()
        require('luasnip.loaders.from_vscode').lazy_load()
      end
    },
  },

  opts = {
    completion = {
      completeopt = vim.o.completeopt,
      keyword_length = 2,
    },

    formatting = {
      format = function(_, item)
        if icons.kinds[item.kind] then
          item.kind = ' ' .. icons.kinds[item.kind] .. item.kind
        end

        return item
      end,
    },
  },

  config = function(_, opts)
    local cmp = require('cmp')
    local luasnip = require('luasnip')

    opts.snippet = function(args)
      luasnip.lsp_expand(args.body)
    end

    local sources = {
      path = {
        name = 'path',
        option = { trailing_slash = true }
      }
    }

    if os.getenv('TMUX') then
      sources.tmux = {
        name = 'tmux',
        option = { all_panes = true },
        keyword_length = 3,
      }
    end

    opts.sources = cmp.config.sources({
      { name = 'nvim_lsp_signature_help' },
      { name = 'nvim_lsp' },
    }, {
      { name = 'luasnip' },
      { name = 'buffer' },
      sources.path,
      sources.tmux,
    }, {
      { name = 'calc' },
      { name = 'emoji' },
    })

    local tabs = {
      ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          if luasnip.expandable() then
            luasnip.expand()
          else
            cmp.confirm({ select = true })
          end
        elseif luasnip.locally_jumpable(1) then
          luasnip.jump(1)
        elseif has_words_before() or vim.api.nvim_get_mode().mode == 'c' then
          cmp.complete()

          if #cmp.get_entries() == 1 then
            cmp.confirm({ select = true })
          end
        else
          fallback()
        end
      end, { 'i', 's', 'c' }),

      ['<S-Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.locally_jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end),
    }

    opts.mapping = cmp.mapping.preset.insert(tabs)

    cmp.setup(opts)

    cmp.setup.cmdline({ '/', '?' }, {
      mapping = cmp.mapping.preset.cmdline(tabs),
      sources = {
        { name = 'buffer' },
        sources.tmux,
      }
    })

    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(tabs),
      matching = { disallow_symbol_nonprefix_matching = false },
      completion = { autocomplete = false },
      sources = {
        { name = 'cmdline' },
        sources.path,
      }
    })

    cmp.setup.filetype('lua', {
      sources = util.merge(opts.sources, {{ name = 'nvim_lua' }})
    })
  end
}
