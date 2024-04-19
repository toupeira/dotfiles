local util = require('util')

local icons = {
  kinds = {
    Array         = " ",
    Boolean       = "󰨙 ",
    Class         = " ",
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
    Interface     = " ",
    Key           = " ",
    Keyword       = " ",
    Method        = "󰊕 ",
    Module        = " ",
    Namespace     = "󰦮 ",
    Null          = " ",
    Number        = "󰎠 ",
    Object        = " ",
    Operator      = " ",
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

    local path = {
      name = 'path',
      option = { trailing_slash = true }
    }

    local tmux
    if os.getenv('TMUX') then
      tmux = {
        name = 'tmux',
        option = { all_panes = true },
        keyword_length = 3,
      }
    end

    opts.sources = cmp.config.sources({
      { name = 'nvim_lsp_signature_help' },
      { name = 'nvim_lsp' },
    }, {
      { name = 'buffer' },
      path,
      tmux,
    }, {
      { name = 'calc' },
      { name = 'emoji' },
    })

    local tab = {
      ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.confirm({ select = true })
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, { 'i', 's', 'c' }),
    }

    opts.mapping = cmp.mapping.preset.insert(tab)

    cmp.setup(opts)

    cmp.setup.cmdline({ '/', '?' }, {
      mapping = cmp.mapping.preset.cmdline(tab),
      sources = {
        { name = 'buffer' },
        tmux,
      }
    })

    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(tab),
      matching = { disallow_symbol_nonprefix_matching = false },
      completion = { autocomplete = false },
      sources = {
        { name = 'cmdline' },
        path,
      }
    })

    cmp.setup.filetype('lua', {
      sources = util.merge(opts.sources, {{ name = 'nvim_lua' }})
    })
  end
}
