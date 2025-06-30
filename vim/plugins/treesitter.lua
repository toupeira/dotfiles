local util = require('util')
local nvomap = util.nvomap

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
        multiwindow = true,
      },
    },

    { 'aaronik/treewalker.nvim',
      cmd = 'Treewalker',
      init = function()
        local function move(action)
          return function()
            vim.cmd.Treewalker(action)
            pcall(vim.cmd.foldopen)
          end
        end

        local descendant, ancestor = util.make_repeatable(move('Right'), move('Left'))
        local next, previous = util.make_repeatable(move('Down'), move('Up'))

        nvomap(']s', descendant, 'Move to descendant node')
        nvomap('[s', ancestor, 'Move to ancestor node')
        nvomap(']S', next, 'Move to next neighbor node')
        nvomap('[S', previous, 'Move to previous neighbor node')
      end
    },
  },

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
    indent = { enable = true },

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
        },
      },

      move = {
        enable = true,
        goto_next_start = {
          [']a'] = { query = '@parameter.inner', desc = 'Next argument' },
          [']m'] = { query = '@class.outer', desc = 'Next module' },
          [']f'] = { query = '@function.outer', desc = 'Next function' },
        },
        goto_next_end = {
          [']A'] = { query = '@parameter.inner', desc = 'End of argument' },
          [']M'] = { query = '@class.outer', desc = 'End of module' },
          [']F'] = { query = '@function.outer', desc = 'End of function' },
        },
        goto_previous_start = {
          ['[a'] = { query = '@parameter.inner', desc = 'Previous argument' },
          ['[m'] = { query = '@class.outer', desc = 'Previous module' },
          ['[f'] = { query = '@function.outer', desc = 'Previous function' },
        },
        goto_previous_end = {
          ['[A'] = { query = '@parameter.inner', desc = 'End of previous argument' },
          ['[M'] = { query = '@class.outer', desc = 'End of previous module' },
          ['[F'] = { query = '@function.outer', desc = 'End of previous function' },
        },
      },
    },
  },

  config = function(_, opts)
    -- define filetype aliases
    vim.treesitter.language.register('yaml', 'eruby.yaml')

    require('nvim-treesitter.configs').setup(opts)

    util.autocmd('FileType', opts.ensure_installed, function()
      vim.wo[0][0].foldmethod = 'expr'
      vim.wo[0][0].foldexpr = 'v:lua.vim.treesitter.foldexpr()'
    end)

    local repeat_move = require('nvim-treesitter.textobjects.repeatable_move')
    nvomap(';', repeat_move.repeat_last_move)
    nvomap('|', repeat_move.repeat_last_move_opposite)
    nvomap('<S-Tab>', '|', { force = true, remap = true })

    nvomap('f', repeat_move.builtin_f_expr, { expr = true, force = true })
    nvomap('F', repeat_move.builtin_F_expr, { expr = true, force = true })
    nvomap('t', repeat_move.builtin_t_expr, { expr = true, force = true })
    nvomap('T', repeat_move.builtin_T_expr, { expr = true, force = true })
  end,
}
