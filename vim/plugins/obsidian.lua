local util = require('util')

return {
  'obsidian-nvim/obsidian.nvim',
  ft = 'markdown',
  cmd = 'Obsidian',

  dependencies = {
    'nvim-lua/plenary.nvim',
  },

  opts = {
    disable_frontmatter = true,
    legacy_commands = false,
    picker = { name = 'fzf-lua' },
    follow_url_func = function(url)
      vim.fn.jobstart({ 'xdg-open', url })
    end,

    ui = {
      enable = false,
    },

    checkbox = {
      order = { ' ', 'x', '-' },
    },

    completion = {
      nvim_cmp = false,
      blink = true,
    },

    workspaces = {
      {
        name = 'notes',
        path = '/slack/documents/Notes',
      },

      {
        name = 'no-vault',
        path = function()
          return assert(vim.fn.getcwd())
        end,
        overrides = {
          disable_frontmatter = true,
          notes_subdir = vim.NIL,
          new_notes_location = 'current_dir',
          daily_notes = { folder = vim.NIL, },
          templates = { folder = vim.NIL },
        },
      },
    },

    callbacks = {
      enter_note = function(_, note)
        local opts = { buffer = note.bufnr, force = true }

        util.nmap('gf', '<Cmd>Obsidian follow_link<CR>', opts, 'Go to file')
        util.nmap('<M-space>', '<Cmd>Obsidian toggle_checkbox<CR>', opts, 'Toggle checkbox')
      end,
    },

    daily_notes = {
      folder = 'Journal',
      date_format = '%Y/%Y-%m-%d %A',
      default_tags = {},
    },
  },
}
