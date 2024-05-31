local util = require('util')

return {
  { 'catppuccin/nvim', name = 'catppuccin', lazy = true },
  { 'folke/tokyonight.nvim', lazy = true },
  { 'marko-cerovac/material.nvim', lazy = true },

  { 'sainnhe/gruvbox-material', lazy = true,
    init = function()
      vim.g.gruvbox_material_background = 'hard'
    end
  },

  { 'EdenEast/nightfox.nvim',
    priority = 1000,

    init = function()
      if vim.o.termguicolors then
        vim.cmd.colorscheme('nordfox')
      end
    end,

    opts = function()
      local shade = function(color)
        return { base = color, bright = color, dim = color }
      end

      return {
        options = {
          module_default = false,
          terminal_colors = false,

          modules = {
            neogit = true,
            neotree = true,
          }
        },

        palettes = {
          nordfox = {
            black   = shade('#191c26'),
            red     = shade('#ff6b83'),
            green   = shade('#C3E88D'),
            yellow  = shade('#ffcb6b'),
            blue    = shade('#82b1ff'),
            magenta = shade('#c792ea'),
            cyan    = shade('#89DDFF'),
            white   = shade('#bfc7d5'),
            orange  = shade('#F78C6C'),
            pink    = shade('#ff869a'),

            comment = '#697098',

            bg0 = '#11131a',  -- Dark bg (status line and float)
            bg1 = '#191c26',  -- Default bg
            bg2 = '#222633',  -- Lighter bg (colorcolm folds)
            bg3 = '#2f3347',  -- Lighter bg (cursor line)
            bg4 = '#3b4048',  -- Conceal, border fg

            fg0 = '#e6e6ff',  -- Lighter fg
            fg1 = '#bfc7d5',  -- Default fg
            fg2 = '#bfc7d5',  -- Darker fg (status line)
            fg3 = '#4b5263',  -- Darker fg (line numbers, fold colums)

            sel0 = '#2f3347', -- Popup bg, visual selection bg
            sel1 = '#939ede', -- Popup sel bg, search bg

            search = {
              bg0 = '#2d402d',
              bg1 = '#5f875f',
              fg0 = '#d7ffaf',
              fg1 = '#ecffd9',
            },

            diff = {
              add = '#1c4428',
              delete = '#993d3d',
              change = '#293f66',
              text = '#3e5f99',
            }
          },
        },

        specs = {
          nordfox = {
            syntax = {
              const = 'red',      -- orange
              keyword = 'red',    -- magenta
              operator = 'cyan',  -- fg2
              preproc = 'yellow', -- pink
            },

            diff = {
              add = 'diff.add',
              delete = 'diff.delete',
              change = 'diff.change',
              text = 'diff.text',
            },

            git = {
              add = 'green',
              removed = 'red',
              changed = 'blue',
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
            Visual = { bg = 'palette.bg4', style = 'bold' },

            NonText = { fg = 'palette.comment' },
            EndOfBuffer = { fg = 'bg4' },
            Folded = { fg = 'palette.comment', bg = 'bg3', style = 'bold' },

            PmenuSel = { bg = 'bg0', style = 'bold' },
            PmenuThumb = { bg = 'bg0' },
            TabLine = { fg = 'palette.comment', bg = 'bg0' },
            TabLineSel = { fg = 'palette.white', bg = 'bg1', style = 'bold' },
            WinBar = { bg = 'bg0' },
            WinSeparator = { fg = 'bg4' },

            Search = { fg = 'palette.search.fg0', bg = 'palette.search.bg0', style = 'bold' },
            CurSearch = { fg = 'palette.search.fg1', bg = 'palette.search.bg1', style = 'bold' },
            IncSearch = { link = 'Search' },
            Substitute = { link = 'Search' },

            DiffDelete = { fg = 'palette.red' },
            DiffText = { style = 'bold' },

            ErrorMsg = { style = 'bold' },
            WarningMsg = { style = 'bold' },
            MoreMsg = { fg = 'diag.hint' },

            -- fix background color for diagnostic signs
            -- TODO: submit upstream?
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
            Title = { fg = 'syntax.string' },
            Todo = { fg = 'diag.warn', bg = 'none', style = 'bold' },

            ['@keyword.function'] = { link = 'Statement' },

            ['@comment.error'] = { fg = 'diag.error', bg = 'none', style = 'bold' },
            ['@comment.warning'] = { fg = 'diag.warn', bg = 'none', style = 'bold' },
            ['@comment.todo'] = { fg = 'diag.warn', bg = 'none', style = 'bold' },
            ['@comment.note'] = { fg = 'diag.hint', bg = 'none', style = 'bold' },

            ['@markup.heading'] = { link = 'Function' },
            ['@markup.italic'] = { style = 'italic' },
            ['@markup.list'] = { link = 'PreProc' },
            ['@markup.raw'] = { fg = 'syntax.string', style = 'NONE' },
            ['@markup.strong'] = { fg = 'palette.yellow', style = 'bold' },

            ['@string.special.url'] = { fg = 'palette.blue', style = 'underline' },

            ['@function.builtin.bash'] = { link = 'Operator' },
            ['@keyword.bash'] = { link = 'Operator' },
            ['@keyword.directive.bash'] = { link = 'Comment' },
            ['@punctuation.bracket.bash'] = { link = 'Operator' },
            ['@punctuation.special.bash'] = { link = 'PreProc' },
            ['@variable.bash'] = { link = 'PreProc' },

            ['@keyword.ruby'] = { link = 'Statement' },

            ['@function.macro.vim'] = { link = 'Macro' },
            ['@keyword.vim'] = { link = 'Statement' },
            ['@variable.builtin.vim'] = { link = 'PreProc' },

            shFunctionKey = { link = 'Statement' },
            shSet = { link = 'Operator' },
            shStatement = { link = 'Operator' },

            rubyCapitalizedMethod = { link = 'Type' },
            rubyInclude = { link = 'Operator' },
            rubyInstanceVariable = { link = 'Keyword' },
            rubyInterpolation = { link = 'Operator' },
            rubyInterpolationDelimiter = { link = 'Keyword' },
            rubyPseudoVariable = { link = 'Special' },
            rubyStringDelimiter = { link = 'String' },
            rubySymbol = { link = 'Special' },

            -- plugins
            CmpItemMenu = { link = 'Comment' },
            CmpItemKindSnippet = { link = 'Function' },

            LazyButtonActive = { link = 'LazyH1' },
            LazyDimmed = { link = 'Comment' },
            LazyProp = { link = 'Comment' },

            MatchParen = { fg = 'none', bg = 'sel0', style = 'bold' },
            MatchWord = { style = 'bold' },

            MiniStarterFooter = { link = 'DiagnosticInfo' },

            NeogitCursorLine = { link = 'MsgArea' },
            NeogitChangeModified = { fg = 'diag.warn', bg = 'diag_bg.warn', style = 'bold' },
            NeogitRecentcommits = { fg = 'diag.info', bg ='diag_bg.info', style = 'bold' },
            NeogitStagedchanges = { fg = 'diag.ok', bg = 'diag_bg.ok', style = 'bold' },
            NeogitUnstagedchanges = { fg = 'diag.error', bg = 'diag_bg.error', style = 'bold' },
            NeogitUntrackedfiles = { link = 'NeogitChangeModified' },
            NeogitUnpulledFrom = { link = 'NeogitChangeModified' },
            NeogitUnmergedInto = { link = 'NeogitChangeModified' },

            NeoTreeGitConflict = { link = 'ErrorMsg' },
            NeoTreeGitModified = { link = 'WarningMsg' },
            NeoTreeGitStaged = { link = 'MoreMsg' },
            NeoTreeGitUnstaged = { link = 'WarningMsg' },
            NeoTreeGitUntracked = { link = 'ErrorMsg' },

            SignifySignAdd    = { fg = 'git.add', bg = 'diff.add' },
            SignifySignDelete = { fg = 'fg0', bg = 'diff.delete' },
            SignifySignChange = { fg = 'git.changed', bg = 'diff.change' },

            WhichKey = { fg = 'palette.yellow', style = 'bold' },
            WhichKeyDesc = { fg = 'palette.green' },
            WhichKeyGroup = { fg = 'palette.blue', style = 'bold' },
          },
        },
      }
    end
  }
}
