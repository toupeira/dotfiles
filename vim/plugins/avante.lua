return {
  'yetone/avante.nvim',
  event = 'VeryLazy',
  build = 'make',

  dependencies = {
    'nvim-treesitter/nvim-treesitter',
    'stevearc/dressing.nvim',
    'nvim-lua/plenary.nvim',
    'MunifTanjim/nui.nvim',
  },

  opts = {
    hints = {
      enabled = false,
    },
    mappings = {
      sidebar = {
        close_from_input = { normal = 'q', insert = '<C-d>' },
      },
    },
    windows = {
      position = 'smart',
      width = 30,
      height = 30,
      input = {
        prefix = '» ',
      },
      sidebar_header = {
        align = 'left',
      },
    },
  },
}
