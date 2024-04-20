return {
  'folke/trouble.nvim',
  event = 'LazyFile',
  branch = 'dev',

  keys = {
    {
      '<Leader>e',
      '<Cmd>Trouble diagnostics toggle filter.buf=0<CR>',
      desc = 'Toggle diagnostics',
    },
  },

  opts = {
    auto_close = true,
    win = { size = 5 },
  }
}
