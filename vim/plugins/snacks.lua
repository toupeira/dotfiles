return {
  'folke/snacks.nvim',
  event = 'LazyFile',

  keys = {
    { '<M-z>', function()
      if vim.bo.filetype == 'fzf' then
        return '<M-z>'
      end

      vim.schedule(require('snacks.terminal').toggle)
    end, mode = { 'n', 't' }, expr = true, desc = 'Toggle terminal' },

    { '<Leader>gx',
      mode = { 'n', 'v' },
      '<Cmd>lua Snacks.gitbrowse.open({ open = function(url) vim.fn.setreg("+", url); vim.print(url); end })<CR>',
      desc = 'Copy Git URL to current location',
    },

    { '<Leader>gX',
      mode = { 'n', 'v' },
      '<Cmd>lua Snacks.gitbrowse.open()<CR>',
      desc = 'Browse Git URL to current location',
    },

    { '<Leader>u', '<Cmd>lua Snacks.picker.undo()<CR>', desc = 'Show undo history' },
  },

  opts = {
    bigfile = {
      size = 1 * 1024 * 1024, -- 1MB
    },

    gitbrowse = {
      notify = false,
      what = 'permalink',

      url_patterns = {
        ['git%..*'] = {
          branch = '/-/tree/{branch}',
          file = '/-/blob/{branch}/{file}#L{line_start}-L{line_end}',
          permalink = '/-/blob/{commit}/{file}#L{line_start}-L{line_end}',
          commit = '/-/commit/{commit}',
        },
        ['gitlab%..*'] = {
          branch = '/-/tree/{branch}',
          file = '/-/blob/{branch}/{file}#L{line_start}-L{line_end}',
          permalink = '/-/blob/{commit}/{file}#L{line_start}-L{line_end}',
          commit = '/-/commit/{commit}',
        },
      },
    },

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

    picker = {
      ui_select = false,

      layout = {
        preset = 'ivy',
        layout = {
          backdrop = true,
          border = 'rounded',
        },
      },

      win = {
        input = {
          keys = {
            ['<F1>'] = { 'toggle_help_input', mode = { 'i', 'n' }},
            ['<Esc>'] = { 'close', mode = { 'i', 'n' }},

            ['<C-g>'] = { 'toggle_ignored', mode = { 'i', 'n' } },
            ['<A-h>'] = false,
            ['<A-i>'] = false,

            ['<C-_>'] = { 'toggle_preview', mode = { 'i', 'n' } },
            ['<A-p>'] = false,

            ['<C-b>'] = { 'list_scroll_up', mode = { 'i', 'n' } },
            ['<C-f>'] = { 'list_scroll_down', mode = { 'i', 'n' } },
            ['<A-b>'] = { 'preview_scroll_up', mode = { 'i', 'n' } },
            ['<A-f>'] = { 'preview_scroll_down', mode = { 'i', 'n' } },
            ['<A-y>'] = { 'preview_scroll_up', mode = { 'i', 'n' } },
            ['<A-e>'] = { 'preview_scroll_down', mode = { 'i', 'n' } },
            ['<C-u>'] = false,
            ['<C-d>'] = false,
          },
        },
      },
    },

    statuscolumn = {
      left = { 'git' },
      right = { 'mark', 'sign', 'fold' },
      folds = { open = true },
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
        keys = {
          gf = false,
        }
      },
    },
  },
}
