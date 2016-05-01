" syntastic syntax checker

if exists('g:loaded_syntastic_elixir_dogma_checker')
    finish
endif
let g:loaded_syntastic_elixir_dogma_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_elixir_dogma_IsAvailable() dict
  call system('fgrep -q :dogma, mix.exs 2>/dev/null')
  if (v:shell_error == 0)
    return 1
  else
    return 0
  endif
endfunction

function! SyntaxCheckers_elixir_dogma_GetLocList() dict
  let makeprg = self.makeprgBuild({
      \ 'args_after': 'dogma --format=flycheck' })

  let errorformat = '%f:%l:%c: %t:%m'

  return SyntasticMake({
      \ 'makeprg': makeprg,
      \ 'errorformat': errorformat})
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
			\ 'filetype': 'elixir',
			\ 'name': 'dogma',
			\ 'exec': 'mix'})

let &cpo = s:save_cpo
unlet s:save_cpo
