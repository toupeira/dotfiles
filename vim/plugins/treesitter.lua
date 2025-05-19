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
        multiwindow = true,
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
    nvomap('<S-Tab>', '|', { force = true, remap = true })

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
      'diff',
      'gdscript',
      'gdshader',
      'git_config',
      'git_rebase',
      'gitattributes',
      'gitcommit',
      'gitignore',
      'html',
      'ini',
      'javascript',
      'json',
      'lua',
      'markdown',
      'markdown_inline',
      'mermaid',
      'python',
      'regex',
      'ruby',
      'toml',
      'typescript',
      'vim',
      'vimdoc',
      'vue',
      'yaml',
      'xml',
    },

    highlight = { enable = true },
    endwise = { enable = true },
    indent = { enable = false },
    matchup = { enable = true },

    refactor = {
      navigation = {
        enable = true,
        keymaps = {
          goto_next_usage = '<C-]>',
          goto_previous_usage = '<C-[>',
        },
      },

      smart_rename = {
        enable = true,
        keymaps = { smart_rename = 'gR' },
      },
    },

    textobjects = {
      select = {
        enable = true,
        lookahead = true,

        keymaps = {
          ['am'] = { query = '@class.outer', desc = 'Select current module' },
          ['im'] = { query = '@class.inner', desc = 'Select current module body' },
          ['af'] = { query = '@function.outer', desc = 'Select current function' },
          ['if'] = { query = '@function.inner', desc = 'Select current function body' },
          ['aa'] = { query = '@parameter.outer', desc = 'Select all arguments' },
          ['ia'] = { query = '@parameter.inner', desc = 'Select current argument' },
        },
      },

      move = {
        enable = true,
        goto_next_start = {
          [']a'] = { query = '@parameter.inner', desc = 'Next argument' },
          [']m'] = { query = '@class.outer', desc = 'Next module' },
          [']f'] = { query = '@function.outer', desc = 'Next function' },
          [']s'] = { query = '@local.scope', query_group = 'locals', desc = 'Next scope' },
        },
        goto_next_end = {
          [']A'] = { query = '@parameter.inner', desc = 'End of argument' },
          [']M'] = { query = '@class.outer', desc = 'End of module' },
          [']F'] = { query = '@function.outer', desc = 'End of function' },
          [']S'] = { query = '@local.scope', query_group = 'locals', desc = 'End of scope' },
        },
        goto_previous_start = {
          ['[a'] = { query = '@parameter.inner', desc = 'Previous argument' },
          ['[m'] = { query = '@class.outer', desc = 'Previous module' },
          ['[f'] = { query = '@function.outer', desc = 'Previous function' },
          ['[s'] = { query = '@local.scope', query_group = 'locals', desc = 'Previous scope' },
        },
        goto_previous_end = {
          ['[A'] = { query = '@parameter.inner', desc = 'End of previous argument' },
          ['[M'] = { query = '@class.outer', desc = 'End of previous module' },
          ['[F'] = { query = '@function.outer', desc = 'End of previous function' },
          ['[S'] = { query = '@local.scope', query_group = 'locals', desc = 'End of previous scope' },
        },
      },
    },
  }
}
