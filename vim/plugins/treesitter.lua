return {
  'nvim-treesitter/nvim-treesitter',
  event = 'LazyFile',
  build = ':TSUpdate',
  dependencies = {
    { 'nvim-treesitter/nvim-treesitter-refactor' },
    { 'nvim-treesitter/nvim-treesitter-textobjects' },
    { 'RRethy/nvim-treesitter-endwise' },

    { 'nvim-treesitter/nvim-treesitter-context',
      opts = {
        max_lines = 3,
        min_window_height = 10,
      }
    },
  },

  config = function(_, opts)
    require('nvim-treesitter.configs').setup(opts)
  end,

  opts = {
    ensure_installed = {
      'bash',
      'comment',
      'css',
      'html',
      'javascript',
      'json',
      'lua',
      'markdown',
      'markdown_inline',
      'python',
      'ruby',
      'typescript',
      'vim',
      'vimdoc',
      'vue',
      'yaml',
    },

    highlight = { enable = true, disable = { 'ruby' }},

    endwise = { enable = true },
    indent = { enable = true },
    matchup = { enable = true },

    refactor = {
      navigation = {
        enable = false,
        keymaps = {
          goto_definition = 'gd',
          goto_next_usage = '<M-*>',
          goto_previous_usage = '<M-#>',
        },
      },

      smart_rename = {
        enable = true,
        keymaps = { smart_rename = '<Leader>dR' },
      },
    },

    textobjects = {
      select = {
        enable = true,
        keymaps = {
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['am'] = '@class.outer',
          ['im'] = '@class.inner',
        },
      },

      move = {
        enable = true,
        goto_next_start = {
          [']f'] = '@function.outer',
          [']m'] = '@class.outer',
        },
        goto_next_end = {
          [']F'] = '@function.outer',
          [']M'] = '@class.outer',
        },
        goto_previous_start = {
          ['[f'] = '@function.outer',
          ['[m'] = '@class.outer',
        },
        goto_previous_end = {
          ['[F'] = '@function.outer',
          ['[M'] = '@class.outer',
        },
      },
    },
  }
}
