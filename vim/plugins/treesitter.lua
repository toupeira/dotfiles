return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  dependencies = {
    { 'nvim-treesitter/nvim-treesitter-textobjects' },
    { 'nvim-treesitter/nvim-treesitter-refactor' },
    { 'andymass/vim-matchup' },
  },

  config = function(_, opts)
    require('nvim-treesitter.configs').setup(opts)
  end,

  opts = {
    ensure_installed = {
      'bash',
      'css',
      'html',
      'javascript',
      'json',
      'lua',
      'markdown',
      'markdown_inline',
      'ruby',
      'typescript',
      'vim',
      'vimdoc',
      'vue',
      'yaml',
    },
    highlight = {
      enable = true,
      disable = { 'bash', 'ruby' },
    },
    indent = { enable = true },
    matchup = { enable = true },
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
    refactor = {
      navigation = { enable = true },
      smart_rename = { enable = true },
    },
  }
}
