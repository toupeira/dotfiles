return {
  'NeogitOrg/neogit',

  cmd = 'Neogit',
  keys = {
    { '<Leader>gs', '<Cmd>Neogit<CR>', desc = 'Open Neogit' },
  },

  opts = {
    disable_hint = true,
    graph_style = 'unicode',
    remember_settings = false,
  }
}
