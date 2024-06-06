local util = require('util')
local nvomap = util.nvomap

return {
  'nvim-treesitter/nvim-treesitter',
  event = 'VeryLazy',
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
    ensure_installed = util.is_sudo and {} or {
      'bash',
      'c',
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

    highlight = { enable = true },
    endwise = { enable = true },
    indent = { enable = false },
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
        lookahead = true,

        keymaps = {
          ['ac'] = { query = '@class.outer', desc = 'Select current class' },
          ['ic'] = { query = '@class.inner', desc = 'Select current class body' },
          ['af'] = { query = '@function.outer', desc = 'Select current function' },
          ['if'] = { query = '@function.inner', desc = 'Select current function body' },
          ['aa'] = { query = '@parameter.outer', desc = 'Select all arguments' },
          ['ia'] = { query = '@parameter.inner', desc = 'Select current argument' },
        },
      },

      move = {
        enable = true,
        goto_next_start = {
          [']a'] = { query = '@parameter.inner', desc = 'Go to next argument' },
          [']c'] = { query = '@class.outer', desc = 'Go to next class' },
          [']f'] = { query = '@function.outer', desc = 'Go to next function' },
        },
        goto_next_end = {
          [']A'] = { query = '@parameter.inner', desc = 'Go to end of argument' },
          [']C'] = { query = '@class.outer', desc = 'Go to end of class' },
          [']F'] = { query = '@function.outer', desc = 'Go to end of function' },
        },
        goto_previous_start = {
          ['[a'] = { query = '@parameter.inner', desc = 'Go to previous argument' },
          ['[c'] = { query = '@class.outer', desc = 'Go to previous class' },
          ['[f'] = { query = '@function.outer', desc = 'Go to previous function' },
        },
        goto_previous_end = {
          ['[A'] = { query = '@parameter.inner', desc = 'Go to end of previous argument' },
          ['[C'] = { query = '@class.outer', desc = 'Go to end of previous class' },
          ['[F'] = { query = '@function.outer', desc = 'Go to end of previous function' },
        },
      },
    },
  }
}
