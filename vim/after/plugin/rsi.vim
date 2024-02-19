" restore default mapping for <C-d>
iunmap <C-d>
cunmap <C-d>

" https://github.com/tpope/vim-rsi/issues/14
iunmap <M-d>
cunmap <M-d>

if !has("gui_running")
  set <F31>=
  silent! unmap! <F31>
  silent! unmap <F31>
endif
