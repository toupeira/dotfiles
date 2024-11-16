return {
  'yetone/avante.nvim',
  event = 'VeryLazy',
  build = 'make',

  dependencies = {
    'nvim-treesitter/nvim-treesitter',
    'stevearc/dressing.nvim',
    'nvim-lua/plenary.nvim',
    'MunifTanjim/nui.nvim',
    'echasnovski/mini.nvim',
  },

  opts = {
    hints = { enabled = false },
  },
}
