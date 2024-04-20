return {
  'NeogitOrg/neogit',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'ibhagwan/fzf-lua',

    { 'sindrets/diffview.nvim',
      lazy = false,
      opts = {
        view = {
          default = { layout = 'diff2_vertical' },
        },
      },
    },
  },

  keys = {
    { '<Leader>gs', '<Cmd>Neogit<CR>', desc = 'Open Neogit' },
  },

  opts = {
    disable_hint = true,
    graph_style = 'unicode',
    remember_settings = false,
  }
}
