local util = require('util')

return { 'folke/which-key.nvim',
  event = 'VeryLazy',

  init = function()
    vim.o.timeoutlen = 1000
    util.nmap('<F1>', ':WhichKey <F1>', { force = true })
    util.nmap('<F1><Enter>', ':Help', 'Open help in a new tab')
  end,

  opts = {
    show_help = false,
    triggers = 'auto',

    labels = {
      ['<Leader>'] = {
        name = '+leader',
        ['<F1>'] = '+help',
        ['d'] = '+lsp',
        ['g'] = '+git',
        ['<Leader>'] = '+resume',
      },
      ['<C-w>'] = '+windows',
      ['<F1>'] = ' help',
      ['['] = '+backward',
      [']'] = '+forward',
      ['\\'] = '+toggle',
      c = '+coerce',
      d = '+diff',
      g = '+go',
      s = '+surround',
      v = '+selection',
      y = '+yank',
      z = '+spelling',
    },

    triggers_nowait = {},

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
  },

  config = function(_, opts)
    local whichkey = require('which-key')
    whichkey.setup(opts)
    whichkey.register(opts.labels)
  end,
}
