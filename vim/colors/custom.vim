""" palenight

let g:palenight_color_overrides = {
  \ 'black':       { 'gui': '#191c26', 'cterm': 'NONE' },
  \ 'visual_grey': { 'gui': '#2f3347', 'cterm': 'NONE' },
  \ 'red':         { 'gui': '#ff6b83', 'cterm': 'NONE' },
\}

runtime colors/palenight.vim

hi! LineNr     guibg=#11131a
hi! SignColumn guibg=#11131a
hi! FoldColumn guibg=#11131a

hi! Folded     guibg=#2f3347 gui=bold

hi! Search     guifg=#ffffff guibg=#6b8e23 gui=bold
hi! IncSearch  guifg=#ffffff guibg=#3a663a gui=bold

hi! PmenuSel   guifg=#000000 guibg=#939ede
hi! PmenuThumb guibg=#697098

hi! DiffAdd    guifg=#d7ffaf guibg=#5f875f
hi! DiffChange guifg=#d7d7ff guibg=#5f5f87
hi! DiffDelete guifg=#ff8080 guibg=#cc6666 gui=bold
hi! DiffText   guifg=#5f5f87 guibg=#81a2be gui=bold

hi! link MatchParen Visual
hi! link QuickFixLine Visual
hi! link VertSplit LineNr

hi! link mkdHeading Constant
hi! link mkdLink Statement
hi! link qfFileName Keyword
hi! link qfLineNr Structure
hi! link rubyModuleName Type

""" badwolf

" let g:badwolf_darkgutter = 1
" runtime colors/badwolf.vim
" let g:airline_theme = 'desertink'

" hi! Visual     gui=bold
" hi! Folded     guifg=#ffa500 guibg=#262626 gui=bold

" hi! Pmenu      guifg=#eeeeee guibg=#444444
" hi! PmenuSel   guifg=#ffffff guibg=#555555 gui=bold
" hi! PmenuSbar  guibg=#666666
" hi! PmenuThumb guibg=#606060

" hi! DiffAdd    guifg=#d7ffaf guibg=#5f875f
" hi! DiffChange guifg=#d7d7ff guibg=#5f5f87
" hi! DiffDelete guifg=#ff8080 guibg=#cc6666 gui=bold
" hi! DiffText   guifg=#5f5f87 guibg=#81a2be gui=bold

" hi! Type       guifg=#ff80a2 gui=bold

" hi! link StatusLine LineNr
" hi! link MatchParen Visual
