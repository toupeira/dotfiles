return {
  { 'catppuccin/nvim', name = 'catppuccin', lazy = true },
  { 'folke/tokyonight.nvim', lazy = true },
  { 'marko-cerovac/material.nvim', lazy = true },

  { 'EdenEast/nightfox.nvim',
    priority = 1000,

    init = function()
      vim.cmd.colorscheme('nordfox')
    end,

    opts = {
      options = {
        module_default = false,
        terminal_colors = false,
      },

      palettes = {
        nordfox = {
          black   = '#191c26',
          red     = '#ff6b83',
          green   = '#C3E88D',
          yellow  = '#ffcb6b',
          blue    = '#82b1ff',
          magenta = '#c792ea',
          cyan    = '#89DDFF',
          white   = '#bfc7d5',
          orange  = '#F78C6C',
          pink    = '#ff869a',

          comment = '#697098',

          bg0 = '#11131a',  -- Dark bg (status line and float)
          bg1 = '#191c26',  -- Default bg
          bg2 = '#2f3347',  -- Lighter bg (colorcolm folds)
          bg3 = '#2f3347',  -- Lighter bg (cursor line)
          bg4 = '#3b4048',  -- Conceal, border fg

          fg0 = '#e6e6ff',  -- Lighter fg
          fg1 = '#bfc7d5',  -- Default fg
          fg2 = '#bfc7d5',  -- Darker fg (status line)
          fg3 = '#4b5263',  -- Darker fg (line numbers, fold colums)

          sel0 = '#2f3347', -- Popup bg, visual selection bg
          sel1 = '#939ede', -- Popup sel bg, search bg

          diff = {
            add = '#1c4428',
            delete = '#993d3d',
            change = '#2f3347',
            text = '#4b5263',
          }
        },
      },

      specs = {
        all = {
          syntax = {
            const = 'cyan',
            ident = 'red',
            keyword = 'red',
            operator = 'cyan',
            preproc = 'yellow',
            -- statement = 'cyan',
          },

          diff = {
            add = 'diff.add',
            delete = 'diff.delete',
            change = 'diff.change',
            text = 'diff.text',
          },
        },
      },

      groups = {
        all = {
          -- editor interface

          LineNr = { bg = 'bg0' },
          SignColumn = { link = 'LineNr' },
          CursorLineNr = { fg = 'palette.comment' },
          CursorLineSign = { link = 'CursorLineNr' },

          NonText = { fg = 'palette.comment' },
          EndOfBuffer = { fg = 'bg4' },
          Folded = { fg = 'palette.comment', style = 'bold' },
          Title = { fg = 'syntax.string' },
          WinBar = { bg = 'bg0' },
          WinSeparator = { fg = 'bg4' },

          Search = { fg = '#d7ffaf', bg = '#2d402d', style = 'bold' },
          CurSearch = { fg = '#ecffd9', bg = '#5f875f', style = 'bold' },
          IncSearch = { link = 'Search' },

          PmenuSel = { fg = 'black' },

          DiffDelete = { fg = 'orange' },
          DiffText = { style = 'bold' },

          MatchParen = { fg = 'none', bg = 'sel0', style = 'bold' },
          MatchWord = { style = 'bold' },

          ErrorMsg = { style = 'bold' },
          WarningMsg = { style = 'bold' },

          -- TODO: fix upstream?
          DiagnosticSignError = { fg = 'diag.error', bg = 'bg0' },
          DiagnosticSignWarn  = { fg = 'diag.warn',  bg = 'bg0' },
          DiagnosticSignInfo  = { fg = 'diag.info',  bg = 'bg0' },
          DiagnosticSignHint  = { fg = 'diag.hint',  bg = 'bg0' },
          DiagnosticSignOk    = { fg = 'diag.ok',    bg = 'bg0' },

          DiagnosticSignCursorError = { fg = 'diag.error', bg = 'bg1' },
          DiagnosticSignCursorWarn  = { fg = 'diag.warn',  bg = 'bg1' },
          DiagnosticSignCursorInfo  = { fg = 'diag.info',  bg = 'bg1' },
          DiagnosticSignCursorHint  = { fg = 'diag.hint',  bg = 'bg1' },
          DiagnosticSignCursorOk    = { fg = 'diag.ok',    bg = 'bg1' },

          -- syntax highlighting

          Delimiter = { fg = 'syntax.statement' },
          Statement = { fg = 'syntax.statement' },
          Todo = { fg = 'diag.warn', bg = 'none', style = 'bold' },

          ['@markup.heading'] = { link = 'Function' },
          ['@markup.italic'] = { style = 'italic' },
          ['@markup.list'] = { link = 'PreProc' },
          ['@markup.raw'] = { fg = 'syntax.string', style = 'NONE' },
          ['@markup.strong'] = { fg = 'palette.yellow', style = 'bold' },

          ['@keyword.ruby'] = { link = 'Statement' },
          ['@keyword.function.ruby'] = { link = 'Statement' },

          ['@function.macro.vim'] = { link = 'Macro' },
          ['@keyword.vim'] = { link = 'Statement' },
          ['@keyword.function.vim'] = { link = 'Statement' },
          ['@variable.builtin.vim'] = { link = 'PreProc' },

          ['@constant.bash'] = { link = 'Identifier' },
          ['@function.builtin.bash'] = { link = 'Operator' },
          ['@keyword.bash'] = { link = 'Operator' },
          ['@keyword.directive.bash'] = { link = 'Comment' },
          ['@keyword.function.bash'] = { link = 'Statement' },
          ['@punctuation.bracket.bash'] = { link = 'Operator' },
          ['@punctuation.special.bash'] = { link = 'PreProc' },
          ['@variable.bash'] = { link = 'PreProc' },

          -- plugins

          LazyButtonActive = { link = 'LazyH1' },
          LazyDimmed = { link = 'Comment' },
          LazyProp = { link = 'Comment' },
        },
      },
    }
  }
}
