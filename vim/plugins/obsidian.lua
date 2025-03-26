return {
  'epwalsh/obsidian.nvim',
  ft = 'markdown',
  keys = {
    { '<Leader>o', '<Cmd>ObsidianQuickSwitch<CR>', desc = 'Open Obsidian note' },
  },
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  opts = {
    disable_frontmatter = true,
    open_app_foreground = true,
    picker = { name = 'fzf-lua' },
    follow_url_func = function(url)
      vim.fn.jobstart({ 'xdg-open', url })
    end,

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
          notes_subdir = vim.NIL,
          new_notes_location = 'current_dir',
          templates = { folder = vim.NIL },
          disable_frontmatter = true,
        },
      },
    },

    ui = {
      checkboxes = {
        [' '] = { char = '󰄱', hl_group = 'ObsidianTodo' },
        ['x'] = { char = '', hl_group = 'ObsidianTodo' },
      }
    },

    mappings = {
      ['gf'] = {
        action = function() return require('obsidian').util.gf_passthrough() end,
        opts = { buffer = true, expr = true },
      },
      ['<CR>'] = {
        action = function() return require('obsidian').util.smart_action() end,
        opts = { buffer = true, expr = true },
      },
      ['<M-space>'] = {
        action = function() return require('obsidian').util.toggle_checkbox() end,
        opts = { buffer = true },
      },
    },

    daily_notes = {
      folder = 'Journal',
      date_format = '%Y/%Y-%m-%d %A',
      default_tags = {},
    },
  },
}
