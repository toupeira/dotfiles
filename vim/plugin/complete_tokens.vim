autocmd FileType ruby,eruby,javascript,css,scss,puppet,c,cs,cpp,php inoremap <buffer> <CR> <C-R>=CompleteTokens()<CR>

let g:complete_tokens = {
  \ 'global': [
    \ [ '({[[^})]*$', '})' ],
    \ [ '(\[[^])]*$', '])' ],
    \ [ '{[^}]*$',    '}'  ],
    \ [ '[[^]]*$',    ']'  ],
    \ [ '([^)]*$',    ')'  ],
  \ ],
  \
  \ 'ruby': [
    \ [ '^\s*\(class\|if\|unless\|begin\|case\|for\|module\|while\|until\|def\)', 'end' ],
    \ [ '\sdo\s*\(|\(,\|\s\|\w\)*|\s*\)\?$', 'end' ],
  \ ],
  \ 'eruby': [
    \ [ '^\s*<%=\?\s*\(class\|if\|unless\|begin\|case\|for\|module\|while\|until\|def\).*%>$', '<% end %>' ],
    \ [ '\sdo\s*\(|\(,\|\s\|\w\)*|\s*\)\?%>$', '<% end %>' ],
    \ [ 'include', 'ruby' ]
  \ ],
\ }

for [s:filetype, s:tokens] in items(g:complete_tokens)
  if s:filetype == 'global'
    continue
  endif

  for [s:pattern, s:token] in s:tokens
    if s:pattern == 'include'
      let s:tokens = s:tokens + g:complete_tokens[s:token]
      break
    endif
  endfor

  let g:complete_tokens[s:filetype] = s:tokens + g:complete_tokens.global
endfor

function! CompleteTokens()
  if has_key(g:complete_tokens, &ft)
    let l:tokens = g:complete_tokens[&ft]
  else
    let l:tokens = g:complete_tokens.global
  endif

  let l:current_line = getline('.')

  for [l:pattern, l:token] in l:tokens
    if match(l:current_line, l:pattern) > -1
      break
    else
      let l:token = ""
    end
  endfor

  let l:keys = neocomplcache#cancel_popup() . "\<CR>"
  if !empty(l:token)
    let l:current_indent = indent('.')
    let l:next_indent = indent(line('.') + 1)
    let l:next_line = getline(line('.') + 1)

    if l:current_indent != l:next_indent || substitute(l:next_line, ' ', '', 'g') != l:token
      let l:keys .= l:token . "\<C-o>O"
    endif
  endif

  return l:keys
endfunction
