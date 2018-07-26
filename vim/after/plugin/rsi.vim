iunmap <M-d>
cunmap <M-d>

if !has("gui_running")
  set <F31>=
  silent! unmap! <F31>
  silent! unmap <F31>
endif
