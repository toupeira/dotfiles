return {
  'epwalsh/obsidian.nvim',
  keys = {
    { '<Leader>o', '<Cmd>ObsidianQuickSwitch<CR>', desc = 'Open Obsidian note' },
  },
  event = {
    'BufReadPre /slack/documents/Notes/*.md',
    'BufNewFile /slack/documents/Notes/*.md',
  },
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  opts = {
    workspaces = {
      {
        name = 'notes',
        path = '/slack/documents/Notes',
      },
    },
    disable_frontmatter = true,
    picker = { name = 'fzf-lua' },
    follow_url_func = function(url)
      vim.fn.jobstart({ 'xdg-open', url })
    end,
    ui = {
      checkboxes = {
        [' '] = { char = '󰄱', hl_group = 'ObsidianTodo' },
        ['x'] = { char = '', hl_group = 'ObsidianTodo' },
      }
    }
  },
}
