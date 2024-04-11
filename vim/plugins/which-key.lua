return { 'folke/which-key.nvim',
  event = 'VeryLazy',

  init = function()
    vim.o.timeoutlen = 1000
  end,

  opts = {
    plugins = {
      presets = {
        operators = false,
        motions = false,
        text_objects = false,
      }
    },

    window = {
      margin = { 0, 0, 1, 0 },
      padding = { 1, 1, 1, 1 },
      winblend = 10,
    },

    layout = {
      height = { min = 1, max = 5 },
      width = { min = 20, max = 30 },
    },

    popup_mappings = {
      scroll_down = '<c-e>',
      scroll_up = '<c-y>',
    },
  }
}
