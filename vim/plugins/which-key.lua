return { 'folke/which-key.nvim',
  event = 'VeryLazy',

  init = function()
    vim.o.timeoutlen = 1000
  end,

  opts = {
    show_help = false,

    plugins = {
      presets = {
        operators = false,
        motions = false,
        text_objects = false,
      }
    },

    window = {
      margin = { 0, 0, 1, 0 },
      padding = { 1, 1, 1, 0 },
      winblend = 10,
    },

    layout = {
      height = { min = 1, max = 10 },
      width = { min = 35, max = 35 },
      align = 'center',
    },

    icons = {
      breadcrumb = '',
      group = ' ',
    },

    popup_mappings = {
      scroll_down = '<c-e>',
      scroll_up = '<c-y>',
    },
  }
}
