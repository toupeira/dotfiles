noremap <silent> ]e :ALENextWrap<CR>
noremap <silent> [e :ALEPreviousWrap<CR>

nmap <silent> <M-j> <Plug>unimpairedMoveDown
nmap <silent> <M-k> <Plug>unimpairedMoveUp
execute 'xmap <silent> <M-j> '.maparg('<Plug>unimpairedMoveSelectionDown') . 'gv'
execute 'xmap <silent> <M-k> '.maparg('<Plug>unimpairedMoveSelectionUp') . 'gv'
