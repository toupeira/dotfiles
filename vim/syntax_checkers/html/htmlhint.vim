"============================================================================
"File:        htmlhint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Markus Koller <markus-koller@gmx.ch>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" Tested with htmlhint 0.9.12
"============================================================================

if exists('g:loaded_syntastic_html_htmlhint_checker')
    finish
endif
let g:loaded_syntastic_html_htmlhint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_html_htmlhint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': '--config ~/.htmlhintrc',
        \ 'args_after': '--format checkstyle' })

    let errorformat = '%f:%t:%l:%c:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'checkstyle',
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'html',
    \ 'name': 'htmlhint',
    \ 'exec': 'htmlhint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
