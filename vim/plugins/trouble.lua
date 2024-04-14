return {
  'folke/trouble.nvim',
  event = 'LazyFile',
  branch = 'dev',

  keys = {
    {
      '<leader>e',
      '<cmd>Trouble diagnostics toggle filter.buf=0<cr>',
      desc = 'Toggle diagnostics',
    },
  },

  opts = {
    auto_close = true,
    win = { size = 5 },
  }
}
