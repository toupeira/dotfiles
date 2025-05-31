return {
  'obsidian-nvim/obsidian.nvim',
  ft = 'markdown',
  cmd = 'Obsidian',
  dependencies = { 'nvim-lua/plenary.nvim' },

  opts = {
    disable_frontmatter = true,
    picker = { name = 'fzf-lua' },
    follow_url_func = function(url)
      vim.fn.jobstart({ 'xdg-open', url })
    end,

    ui = {
      enable = false,
      checkboxes = {
        ['x'] = { char = '󰄲', hl_group = 'Keyword' },
        [' '] = { char = '󰄱', hl_group = 'Keyword' },
      },
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
