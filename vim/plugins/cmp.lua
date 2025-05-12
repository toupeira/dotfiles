local util = require('util')

local icons = {
  kinds = {
    Array         = '',
    Boolean       = '󰨙',
    Class         = '󰆧',
    Codeium       = '󰘦',
    Color         = '',
    Control       = '',
    Collapsed     = '',
    Constant      = '󰏿',
    Constructor   = '',
    Copilot       = '',
    Enum          = '',
    EnumMember    = '',
    Event         = '',
    Field         = '',
    File          = '',
    Folder        = '',
    Function      = '󰊕',
    Interface     = '',
    Key           = '󰌋',
    Keyword       = '',
    Method        = '󰊕',
    Module        = '',
    Namespace     = '󰦮',
    Null          = '󰟢',
    Number        = '󰎠',
    Object        = '',
    Operator      = '󰆕',
    Package       = '',
    Property      = '',
    Reference     = '',
    Snippet       = '',
    String        = '',
    Struct        = '󰆼',
    TabNine       = '󰏚',
    Text          = '',
    TypeParameter = '󰗴',
    Unit          = '',
    Value         = '',
    Variable      = '󰀫',
  },
}

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
end

return {
  'hrsh7th/nvim-cmp',
  enabled = false,
  event = 'VeryLazy',
  dependencies = {
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-calc',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'hrsh7th/cmp-path',

    { 'andersevenrud/cmp-tmux',
      cond = util.is_tmux,
    },
  },

  opts = {
    completion = {
      completeopt = vim.o.completeopt,
      keyword_length = 2,
    },

    formatting = {
      fields = { 'kind', 'abbr', 'menu' },
      format = function(_, item)
        local info = util.join({
          item.kind ~= 'Text' and item.kind or '',
          item.menu,
        }, ' ')

        item.menu = info ~= '' and '   ' .. info
        item.kind = icons.kinds[item.kind]

        return item
      end,
    },
  },

  config = function(_, opts)
    local cmp = require('cmp')

    opts.window = {
      completion = { col_offset = -2 },
      documentation = util.merge(cmp.config.window.bordered(), { max_width = 80 }),
    }

    local sources = {
      path = {
        name = 'path',
        option = { trailing_slash = true }
      },
      buffer = {
        name = 'buffer',
        option = {
          get_bufnrs = function()
            return vim.tbl_filter(
              function(buf)
                return vim.fn.buflisted(buf) == 1 and
                       vim.fn.bufloaded(buf) == 1
              end,
              vim.api.nvim_list_bufs()
            )
          end
        }
      },
    }

    if util.is_tmux then
      sources.tmux = {
        name = 'tmux',
        option = {
          all_panes = true,
          trigger_characters_ft = { markdown = {} },
        },
      }
    end

    opts.sources = cmp.config.sources({
      { name = 'nvim_lsp_signature_help' },
      { name = 'nvim_lsp' },
    }, {
      sources.buffer,
      sources.path,
      sources.tmux,
    }, {
      { name = 'calc' },
    })

    local insert_mappings = {
      ['<M-e>'] = cmp.mapping.scroll_docs(1),
      ['<M-y>'] = cmp.mapping.scroll_docs(-1),
      ['<M-f>'] = cmp.mapping.scroll_docs(8),
      ['<M-b>'] = cmp.mapping.scroll_docs(-8),

      ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.confirm({ select = true })
        elseif has_words_before() or vim.api.nvim_get_mode().mode == 'c' then
          cmp.complete()

          if #cmp.get_entries() == 1 then
            cmp.confirm({ select = true })
          end
        else
          fallback()
        end
      end),

      ['<S-Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        else
          fallback()
        end
      end),
    }

    opts.mapping = cmp.mapping.preset.insert(insert_mappings)

    cmp.setup(opts)

    cmp.setup.filetype('lua', {
      sources = util.merge(opts.sources, {{ name = 'nvim_lua' }})
    })
  end
}
