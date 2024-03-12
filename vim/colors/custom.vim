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

hi! DiffAdd     guifg=#d7ffaf guibg=#5f875f
hi! DiffChange  guifg=#d7d7ff guibg=#5f5f87
hi! DiffDelete  guifg=#ff8080 guibg=#cc6666 gui=bold
hi! DiffText    guifg=#5f5f87 guibg=#81a2be gui=bold

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
