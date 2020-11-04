function! ale#handlers#sh#GetShellType(buffer) abort
  let l:bang_line = get(getbufline(a:buffer, 1), 0, '')

  let l:command = ''

  " Take the shell executable from the hashbang, if we can.
  if l:bang_line[:1] is# '#!'
    " Remove options like -e, etc.
    let l:command = substitute(l:bang_line, ' --\?[a-zA-Z0-9]\+', '', 'g')
  endif

  for l:possible_shell in ['bash', 'dash', 'ash', 'tcsh', 'csh', 'zsh', 'ksh', 'sh']
    if l:command =~# l:possible_shell . '\s*$'
      return l:possible_shell
    endif
  endfor

  if getbufvar(a:buffer, 'is_bash', 0)
    return 'bash'
  elseif getbufvar(a:buffer, 'is_sh', 0)
    return 'sh'
  elseif getbufvar(a:buffer, 'is_kornshell', 0)
    return 'ksh'
  endif

  return ''
endfunction
