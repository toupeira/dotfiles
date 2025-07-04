local util = require('util')

local function hide_source(_, items)
  for _, item in ipairs(items) do
    item.source_name = ''
  end
  return items
end

return {
  'saghen/blink.cmp',
  version = '1.*',
  event = 'VeryLazy',

  dependencies = {
    { 'mgalliou/blink-cmp-tmux', cond = util.is_tmux },
  },

  opts = {
    -- feels awkward because completeopt=longest isn't supported
    -- https://github.com/Saghen/blink.cmp/issues/337
    cmdline = { enabled = false },

    keymap = {
      preset = 'super-tab',

      -- mapped with `mini.keymap`
      ['<Tab>'] = {},
      ['<S-Tab>'] = {},

      ['<C-Space>'] = {},
      ['<C-n>'] = { 'show', 'select_next', 'fallback_to_mappings' },
      ['<C-_>'] = { 'show_documentation', 'hide_documentation' },
      ['<M-b>'] = { 'scroll_documentation_up', 'fallback' },
      ['<M-f>'] = { 'scroll_documentation_down', 'fallback' },
      ['<M-e>'] = { function(cmp) cmp.scroll_documentation_down(1) end, 'fallback' },
      ['<M-y>'] = { function(cmp) cmp.scroll_documentation_up(1) end, 'fallback' },
      ['<M-t>'] = (util.is_tmux and { function(cmp) cmp.show({ providers = { 'tmux' }}) end } or { 'fallback' }),
    },

    completion = {
      keyword = { range = 'full' },
      trigger = { show_in_snippet = false },

      menu = {
        max_height = 10,
        winblend = vim.o.pumblend,

        draw = {
          columns = {
            { 'kind_icon' },
            { 'label', 'label_description', gap = 1 },
            { 'source_name' },
          },
          treesitter = {
            'snippets',
            'lsp',
            'omni',
          },
        },
      },

      documentation = {
        window = { border = 'rounded' },
      },
    },

    fuzzy = {
      sorts = { 'exact', 'score', 'sort_text' },
    },

    signature = {
      enabled = true,
      window = { border = 'rounded' },
    },

    sources = {
      min_keyword_length = 2,

      default = function()
        return {
          'path',
          'buffer',
          'snippets',
          #util.lsp_clients() > 0 and 'lsp' or 'omni'
        }
      end,

      providers = {
        path = {
          enabled = function()
            return vim.bo.filetype ~= 'codecompanion'
          end,
          transform_items = hide_source,
          opts = {
            get_cwd = function(_)
              return vim.fn.getcwd()
            end,
          },
        },

        buffer = {
          transform_items = hide_source,
          opts = {
            get_bufnrs = function()
              return vim.tbl_filter(
                function(buf)
                  return vim.fn.buflisted(buf) == 1 and
                         vim.fn.bufloaded(buf) == 1
                end,
                vim.api.nvim_list_bufs()
              )
            end,
          },
        },

        tmux = {
          enabled = util.is_tmux,
          module = 'blink-cmp-tmux',
          name = 'tmux',
          opts = {
            all_panes = true,
            capture_history = true,
          },
        },
      },
    },
  },
}
