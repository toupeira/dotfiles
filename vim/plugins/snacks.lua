return {
  'folke/snacks.nvim',
  lazy = false,

  keys = {
    { '<M-z>', function()
      if vim.bo.filetype == 'fzf' then
        return '<M-z>'
      end

      vim.schedule(Snacks.terminal.toggle)
    end, mode = { 'n', 't' }, expr = true, desc = 'Toggle terminal' },
  },

  opts = {
    indent = {
      animate = { enabled = false },

      chunk = {
        enabled = true,
        char = {
          corner_top = '╭',
          corner_bottom = '╰',
          arrow = '─',
        }
      },
    },

    input = {},

    styles = {
      input = {
        title_pos = 'left',
        relative = 'cursor',
        row = 1,
        col = 0,
      },

      terminal = {
        position = 'bottom',
        height = 0.3,
        bo = { filetype = 'terminal' },
        wo = { winbar = '' },
      },
    },
  },
}
