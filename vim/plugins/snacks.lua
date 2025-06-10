return {
  'folke/snacks.nvim',
  event = 'VeryLazy',

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

    input = {
      icon = '',
    },

    styles = {
      input = {
        title_pos = 'left',
        relative = 'cursor',
        row = 1,
        col = -2,

        keys = {
          i_ctrl_c = { '<C-c>', 'cancel', mode = 'i' },
        },
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
