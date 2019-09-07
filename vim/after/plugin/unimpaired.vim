noremap <silent> ]e :ALENextWrap<CR>
noremap <silent> ]E :ALELast<CR>
noremap <silent> [e :ALEPreviousWrap<CR>
noremap <silent> [E :ALEFirst<CR>

nmap <silent> <M-j> <Plug>unimpairedMoveDown
nmap <silent> <M-k> <Plug>unimpairedMoveUp
execute 'xmap <silent> <M-j> '.maparg('<Plug>unimpairedMoveSelectionDown') . 'gv'
execute 'xmap <silent> <M-k> '.maparg('<Plug>unimpairedMoveSelectionUp') . 'gv'
