let g:palenight_color_overrides = {
  \ 'black':       { 'gui': '#191c26', 'cterm': 'NONE' },
  \ 'visual_grey': { 'gui': '#2f3347', 'cterm': 'NONE' },
  \ 'red':         { 'gui': '#ff6b83', 'cterm': 'NONE' },
\}

runtime colors/palenight.vim

hi! LineNr      guibg=#11131a
hi! SignColumn  guibg=#11131a
hi! FoldColumn  guibg=#11131a

hi! Folded      guibg=#2f3347 gui=bold

hi! Search      guifg=#d7ffaf guibg=#2d402d gui=bold
hi! IncSearch   guifg=#d7ffaf guibg=#2d402d gui=bold
hi! CurSearch   guifg=#ecffd9 guibg=#5f875f gui=bold

hi! PmenuSel    guifg=#000000 guibg=#939ede
hi! PmenuThumb  guibg=#697098

hi! DiffAdd     guifg=#ccffcc guibg=#476647 gui=bold
hi! DiffDelete  guifg=#ff6666 guibg=#993d3d
hi! DiffChange  guifg=#ccccff guibg=#595980
hi! DiffText    guifg=#e6e6ff guibg=#8686bf gui=bold

hi! link Delimiter Statement
hi! link MatchParen Visual
hi! link QuickFixLine Visual
hi! link ExtraWhitespace Visual
hi! link VertSplit LineNr

hi! link mkdHeading Constant
hi! link mkdLink Statement
hi! link qfFileName Keyword
hi! link qfLineNr String
hi! link rspecGroupMethods Statement
hi! link rspecBeforeAndAfter Identifier
hi! link rubyModuleName Type
hi! link TSKeyword Statement
hi! link TSLabel Identifier
