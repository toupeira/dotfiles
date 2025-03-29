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
      },
      init = function()
        util.nmap('<C-p>', function()
          require('treesitter-context').go_to_context(vim.v.count1)
        end)
      end
    },
  },

  config = function(_, opts)
    require('nvim-treesitter.configs').setup(opts)

    local repeat_move = require('nvim-treesitter.textobjects.repeatable_move')
    nvomap(';', repeat_move.repeat_last_move_next)
    nvomap('|', repeat_move.repeat_last_move_previous)
    nvomap('<S-Tab>', repeat_move.repeat_last_move_previous, { force = true })

    nvomap('f', repeat_move.builtin_f_expr, { expr = true, force = true })
    nvomap('F', repeat_move.builtin_F_expr, { expr = true, force = true })
    nvomap('t', repeat_move.builtin_t_expr, { expr = true, force = true })
    nvomap('T', repeat_move.builtin_T_expr, { expr = true, force = true })
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

    highlight = { enable = true, additional_vim_regex_highlighting = { 'ruby' } },
    endwise = { enable = true },
    indent = { enable = false },
    matchup = { enable = true },

    refactor = {
      navigation = {
        enable = true,
        keymaps = {
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
          [']s'] = { query = '@local.scope', query_group = 'locals', desc = 'Go to next scope' },
        },
        goto_next_end = {
          [']A'] = { query = '@parameter.inner', desc = 'Go to end of argument' },
          [']C'] = { query = '@class.outer', desc = 'Go to end of class' },
          [']F'] = { query = '@function.outer', desc = 'Go to end of function' },
          [']S'] = { query = '@local.scope', query_group = 'locals', desc = 'Go to end of scope' },
        },
        goto_previous_start = {
          ['[a'] = { query = '@parameter.inner', desc = 'Go to previous argument' },
          ['[c'] = { query = '@class.outer', desc = 'Go to previous class' },
          ['[f'] = { query = '@function.outer', desc = 'Go to previous function' },
          ['[s'] = { query = '@local.scope', query_group = 'locals', desc = 'Go to previous scope' },
        },
        goto_previous_end = {
          ['[A'] = { query = '@parameter.inner', desc = 'Go to end of previous argument' },
          ['[C'] = { query = '@class.outer', desc = 'Go to end of previous class' },
          ['[F'] = { query = '@function.outer', desc = 'Go to end of previous function' },
          ['[S'] = { query = '@local.scope', query_group = 'locals', desc = 'Go to end of previous scope' },
        },
      },
    },
  }
}
