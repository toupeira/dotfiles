return {
  'NeogitOrg/neogit',

  cmd = {
    'Neogit',
    'NeogitCommit',
    'NeogitLogCurrent',
  },

  keys = {
    { '<Leader>gs', '<Cmd>Neogit<CR>', desc = 'Open Neogit' },
  },

  opts = {
    console_timeout = 250,
    disable_hint = true,
    graph_style = 'unicode',
    kind = 'split',
    remember_settings = false,

    mappings = {
      commit_editor = {
        ['<C-g><C-g>'] = 'Submit',
        ['<C-g><C-k>'] = 'Abort',
        ['<C-p>'] = 'PrevMessage',
        ['<C-n>'] = 'NextMessage',
        ['<C-g><C-r>'] = 'ResetMessage',

        ['<c-c><c-c>'] = false,
        ['<c-c><c-k>'] = false,
        ['<m-p>'] = false,
        ['<m-n>'] = false,
        ['<m-r>'] = false,
      },

      rebase_editor = {
        ['<C-g><C-g>'] = 'Submit',
        ['<C-g><C-k>'] = 'Abort',

        ['<c-c><c-c>'] = false,
        ['<c-c><c-k>'] = false,
      },
    },
  },
}
