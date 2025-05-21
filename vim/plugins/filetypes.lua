return {
  { 'antonk52/markdowny.nvim', ft = { 'markdown' }, config = true },
  { 'elixir-lang/vim-elixir',  ft = { 'elixir', 'eelixir' }},
  { 'habamax/vim-godot',       ft = { 'gdscript', 'gsl' }},
  { 'hail2u/vim-css3-syntax',  ft = { 'css', 'scss' }},
  { 'hashivim/vim-terraform',  ft = { 'terraform', 'hcl' }},

  { 'catgoose/nvim-colorizer.lua',
    ft = { 'css', 'scss', 'lua' },
    cmd = 'ColorizerToggle',
    keys = {
      { '<LocalLeader>c', '<Cmd>ColorizerToggle<CR>', desc = 'Toggle colorizer' },
    },
    opts = function(plugin)
      return {
        lazy_load = true,
        filetypes = plugin.ft,
        user_default_options = {
          names = false,
        },
      }
    end
  },

  { 'iamcco/markdown-preview.nvim',
    build = function() vim.fn['mkdp#util#install']() end,
    ft = { 'markdown' },
    cmd = {
      'MarkdownPreviewToggle',
      'MarkdownPreview',
      'MarkdownPreviewStop',
    },
    keys = {
      { '<LocalLeader>P', '<Cmd>MarkdownPreviewToggle<CR>', ft = 'markdown', desc = 'Toggle Markdown preview' },
    },
  },

  { 'OXY2DEV/helpview.nvim',
    ft = { 'help' },
  },

  { 'OXY2DEV/markview.nvim',
    ft = { 'markdown', 'codecompanion' },
    cmd = { 'Markview' },
    opts = function(plugin)
      return {
        preview = {
          filetypes = plugin.ft,
          ignore_buftypes = {},
          modes = { 'n', 'no', 'c', 'i' },
          hybrid_modes = { 'n', 'i' },
          linewise_hybrid_mode = true,
        },

        markdown = {
          list_items = {
            shift_width = 2,
            marker_minus       = { add_padding = false, text = '-' },
            marker_plus        = { add_padding = false, text = '+' },
            marker_star        = { add_padding = false, text = '*' },
            marker_dot         = { add_padding = false },
            marker_parenthesis = { add_padding = false },
          },
        },

        markdown_inline = {
          checkboxes = {
            checked = { text = '󰄲', hl = 'Keyword', scope_hl = 'MarkviewCheckboxStriked' },
            unchecked = { text = '󰄱', hl = 'Keyword', scope_hl = '' },
          },
        },
      }
    end
  },

  { 'stevearc/quicker.nvim',
    ft = 'qf',

    opts = {
      follow = { enabled = true },

      keys = {
        { '>', '<Cmd>lua require("quicker").expand({ before = 3, after = 3, add_to_existing = true })<CR>', desc = 'Expand quickfix context' },
        { '<', '<Cmd>lua require("quicker").collapse()<CR>', desc = 'Reduce quickfix context' },
      },

      max_filename_width = function()
        return math.floor(math.min(95, vim.o.columns / 3))
      end,
    },
  },

  { 'vim-ruby/vim-ruby',
    ft = 'ruby',
    init = function()
      vim.g.ruby_operators = 1
      vim.g.ruby_pseudo_operators = 1
      vim.g.ruby_no_expensive = 1

      vim.g.ruby_indent_block_style = 'do'
      vim.g.ruby_indent_assignment_style = 'variable'
      vim.g.ruby_indent_hanging_elements = 0
    end
  },
}
