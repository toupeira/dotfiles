"============================================================================
"File:        elm.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Markus Koller <markus-koller@gmx.ch>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" Tested with elm/elm-make 0.16.0
"============================================================================

if exists('g:loaded_syntastic_elm_elm_make_checker')
    finish
endif
let g:loaded_syntastic_elm_elm_make_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_elm_elm_make_GetLocList() dict
    call elm#Make()
    let fixes = getqflist()
    call setqflist([])
    cclose

    return fixes
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'elm',
    \ 'name': 'elm_make',
    \ 'exec': 'elm-make'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
