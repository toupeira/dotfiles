-- vim: foldmethod=marker foldlevel=0

return {
  { 'catppuccin/nvim', name = 'catppuccin', lazy = true },
  { 'folke/tokyonight.nvim', lazy = true },
  { 'marko-cerovac/material.nvim', lazy = true },

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
        options = { -- {{{
          module_default = false,
          terminal_colors = false,

          modules = {
            blink = true,
            neotree = true,
          }
        }, -- }}}

        palettes = { -- {{{
          nordfox = {
            black   = shade('#191c26'),
            red     = shade('#ff6b83'),
            green   = shade('#c3e88d'),
            yellow  = shade('#ffcb6b'),
            blue    = shade('#82b1ff'),
            magenta = shade('#c792ea'),
            cyan    = shade('#89ddff'),
            white   = shade('#bfc7d5'),
            orange  = shade('#f78c6c'),
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
              delete = '#401a1b',
              delete_bright = '#80302f',
              change = '#293f66',
              text = '#3e5f99',
            }
          },
        }, -- }}}

        specs = { -- {{{
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
        }, -- }}}

        groups = {
          nordfox = {
            -- editor interface {{{
            LineNr = { bg = 'bg0' },
            SignColumn = { link = 'LineNr' },
            CursorLine = { link = 'Visual' },
            CursorLineNr = { fg = 'palette.comment' },
            CursorLineSign = { link = 'CursorLineNr' },
            Visual = { style = 'bold' },

            NonText = { fg = 'palette.comment' },
            EndOfBuffer = { fg = 'bg4' },
            Folded = { fg = 'palette.sel1', bg = 'bg3', style = 'bold' },

            Pmenu = { bg = 'bg0' },
            PmenuKind = { fg = 'palette.blue' },
            PmenuExtra = { fg = 'palette.comment' },
            PmenuSel = { fg = 'white', bg = 'black', style = 'bold' },
            PmenuThumb = { bg = 'bg2' },

            TabLine = { fg = 'palette.comment', bg = 'bg0' },
            TabLineSel = { fg = 'palette.white', bg = 'bg1', style = 'bold' },
            WinBar = { bg = 'bg0' },
            WinSeparator = { bg = 'bg0', fg = 'bg3' },
            FloatBorder = { link = 'WinSeparator' },

            Search = { fg = 'palette.search.fg0', bg = 'palette.search.bg0', style = 'bold' },
            CurSearch = { fg = 'palette.search.fg1', bg = 'palette.search.bg1', style = 'bold' },
            IncSearch = { link = 'Search' },
            Substitute = { link = 'Search' },

            DiffAdd = { style = 'bold' },
            DiffText = { style = 'bold' },
            DiffDelete = { fg = 'palette.red', bg = 'palette.diff.delete_bright' },
            diffAdded = { fg = 'palette.green', bg = 'diff.add', style = 'bold' },
            diffChanged = { fg = 'palette.blue', bg = 'diff.change' },
            diffRemoved = { fg = 'palette.red', bg = 'diff.delete' },

            ErrorMsg = { style = 'bold' },
            WarningMsg = { style = 'bold' },
            MoreMsg = { fg = 'diag.hint' },
            -- }}}

            -- syntax highlighting {{{
            Delimiter = { fg = 'syntax.statement' },
            Statement = { fg = 'syntax.statement' },
            Title = { fg = 'syntax.string' },
            Todo = { fg = 'diag.warn', bg = 'none', style = 'bold' },

            ['@keyword.function'] = { link = 'Statement' },

            ['@comment.error'] = { fg = 'diag.error', bg = 'none', style = 'bold' },
            ['@comment.warning'] = { fg = 'diag.warn', bg = 'none', style = 'bold' },
            ['@comment.todo'] = { fg = 'diag.warn', bg = 'none', style = 'bold' },
            ['@comment.note'] = { fg = 'diag.hint', bg = 'none', style = 'bold' },

            ['@markup.heading'] = { fg = 'palette.blue', style = 'bold' },
            ['@markup.italic'] = { style = 'italic' },
            ['@markup.link.url'] = { fg = 'palette.blue', style = 'underline' },
            ['@markup.list'] = { fg = 'palette.yellow' },
            ['@markup.math'] = { fg = 'palette.magenta', style = 'bold' },
            ['@markup.raw'] = { fg = 'syntax.string', style = 'NONE' },
            ['@markup.strong'] = { fg = 'palette.yellow', style = 'bold' },

            ['@string.special.url'] = { link = '@markup.link.url' },
            -- }}}

            -- filetypes {{{
            ['@function.builtin.bash'] = { link = 'Operator' },
            ['@keyword.bash'] = { link = 'Operator' },
            ['@keyword.directive.bash'] = { link = 'Comment' },
            ['@punctuation.bracket.bash'] = { link = 'Operator' },
            ['@punctuation.special.bash'] = { link = 'PreProc' },
            ['@variable.bash'] = { link = 'PreProc' },

            ['@string.special.path.diff'] = { link = '@markup.strong' },

            ['@keyword.gitcommit'] = { link = 'Statement' },
            ['@markup.heading.gitcommit'] = { link = 'Bold' },
            ['@markup.link.gitcommit'] = { fg = 'palette.green', style = 'bold' },
            ['@string.special.path.gitcommit'] = { link = '@markup.strong' },
            ['@variable.parameter.gitcommit'] = { link = 'String' },

            ['@keyword.git_rebase'] = { link = '@markup.strong' },
            ['@constant.git_rebase'] = { link = 'Statement' },
            ['@spell.git_rebase'] = { link = 'Bold' },

            ['@markup.heading.ini'] = { link = '@markup.strong' },

            ['@markup.link.label.markdown_inline'] = { link = '@markup.link.url' },

            ['@keyword.ruby'] = { link = 'Statement' },
            ['@keyword.type.ruby'] = { link = 'Statement' },

            ['@function.macro.vim'] = { link = 'Macro' },
            ['@keyword.vim'] = { link = 'Statement' },
            ['@variable.builtin.vim'] = { link = 'PreProc' },
            -- }}}

            -- plugins {{{
            BlinkCmpLabelMatch = { fg = 'palette.search.fg0', style = 'bold' },
            BlinkCmpMenuBorder = { link = 'BlinkCmpDocBorder' },
            BlinkCmpSignatureHelpBorder = { link = 'BlinkCmpDocBorder' },

            CodeCompanionChatTool = { fg = 'diag.info', bg = 'diag_bg.info', style = 'bold' },
            CodeCompanionChatToolGroup = { fg = 'diag.error', bg = 'diag_bg.error', style = 'bold' },
            CodeCompanionChatVariable = { fg = 'diag.hint', bg = 'diag_bg.hint', style = 'bold' },

            FlashCurrent = { link = 'CurSearch' },
            FlashLabel = { fg = 'palette.black', bg = 'palette.yellow', style = 'bold' },

            FugitiveblameAnnotation = { link = 'StatusLine' },

            FzfLuaBorder = { link = 'FloatBorder' },
            FzfLuaTitle = { link = '@markup.heading' },
            FzfLuaPreviewTitle = { link = 'Title' },

            GitSignsAdd    = { fg = 'git.add', bg = 'diff.add' },
            GitSignsChange = { fg = 'git.changed', bg = 'diff.change' },
            GitSignsDelete = { fg = 'fg0', bg = 'diff.delete' },

            LazyButtonActive = { link = 'LazyH1' },
            LazyDimmed = { link = 'Comment' },
            LazyProp = { link = 'Comment' },

            MarkviewPalette0Fg = { link = 'MarkviewPalette1Fg' },

            MatchParen = { fg = 'none', bg = 'sel0', style = 'bold' },
            MatchWord = { style = 'bold' },

            MiniJump = { link = 'Visual' },
            MiniStarterFooter = { link = 'DiagnosticInfo' },
            MiniTrailspace = { bg = 'bg2' },

            NeogitChangeModified = { fg = 'diag.warn', bg = 'diag_bg.warn', style = 'bold' },
            NeogitNormal = { link = 'NormalFloat' },
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

            QuickFixLineNr = { link = 'Visual' },

            SnacksIndent = { fg = 'bg2' },
            SnacksIndentScope = { fg = 'fg3' },
            SnacksInputBorder = { link = 'FloatBorder' },
            SnacksInputNormal = { link = 'NormalFloat' },
            SnacksInputIcon = { link = 'DiagnosticInfo' },
            SnacksInputTitle = { link = 'FloatTitle' },

            TreesitterContextBottom = { style = 'underline', sp = 'bg3' },
            TSDefinition = { link = 'Visual' },
            -- }}}
          },
        },
      }
    end,
  },
}
