local util = require('util')

return {
  'nvimdev/dashboard-nvim',
  enabled = false,
  event = 'VimEnter',

  opts = {
    hide = { statusline = false },
    shortcut_type = 'number',

    config = {
      project = { limit = 5 },
      mru = { limit = 10 },
      footer = {},

      shortcut = {
        { key = 'e', action = 'enew', group = '@property', desc = 'Edit new file' },
        { key = 'i', action = function () vim.cmd.enew(); vim.cmd.startinsert() end, group = '@string', desc = 'Insert mode' },
        { key = 'q', action = 'quit', group = 'DiagnosticWarn', desc = 'Quit' },
      },

      header = {
        [[     __                _           ]],
        [[  /\ \ \___  _____   _(_)_ __ ___  ]],
        [[ /  \/ / _ \/ _ \ \ / / | '_ ` _ \ ]],
        [[/ /\  /  __/ (_) \ V /| | | | | | |]],
        [[\_\ \/ \___|\___/ \_/ |_|_| |_| |_|]],
        [[                                   ]],
      },
    }
  },

  init = function()
    util.autocmd('FileType', 'dashboard', function()
      vim.b.minitrailspace_disable = true
    end)

    util.hl_link('DashboardHeader', 'Title')
    util.hl_link('DashboardProjectTitle', 'Statement')
    util.hl_link('DashboardMruTitle', 'Statement')
    util.hl_link('DashboardFiles', 'String')
  end
}
