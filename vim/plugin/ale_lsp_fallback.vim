if &cp || exists('g:loaded_ale_lsp_fallback')
	finish
endif

let g:loaded_ale_lsp_fallback = 1

function! s:on_ready(linter, lsp_details) abort
	let l:id = a:lsp_details.connection_id
	let l:buffer = a:lsp_details.buffer

	if ale#lsp#HasCapability(l:id, 'definition')
		call setbufvar(l:buffer, 'use_ale_lsp_fallback', 0)
	endif
endfunction

function! s:ale_lint_post() abort
	let l:buffer = bufnr('')
	let l:Callback = function('s:on_ready')

	if getbufvar(l:buffer, 'checked_ale_lsp_fallback', 0)
		return
	else
		call setbufvar(l:buffer, 'checked_ale_lsp_fallback', 1)
	endif

	for l:linter in ale#linter#Get(getbufvar(l:buffer, '&filetype'))
		if !empty(l:linter.lsp)
			call ale#lsp_linter#StartLSP(l:buffer, l:linter, l:Callback)
		endif
	endfor
endfunction

augroup ALELSPFallback
	autocmd!
	autocmd User ALELintPre call s:ale_lint_post()
augroup END

function! ALEFallback(lsp, fallback) abort
	let l:buffer = bufnr('')

	try
		if getbufvar(l:buffer, 'use_ale_lsp_fallback', 1)
			execute a:fallback
		else
			execute a:lsp
		endif
	catch
		execute 'echohl ErrorMsg | echomsg v:exception | echohl None'
	endtry
endfunction
