return {
  'dense-analysis/ale',

  init = function()
    vim.cmd([[
      let g:ale_use_neovim_diagnostics_api = 1
      let g:ale_set_loclist = 0
      let g:ale_set_quickfix = 0
      let g:ale_echo_cursor = 0
      let g:ale_virtualtext_cursor = 'disabled'

      let g:ale_lint_on_insert_leave = 1
      let g:ale_lint_on_text_changed = 'normal'
      let g:ale_fix_on_save = 1
      let g:ale_fix_on_save_ignore = ['rubocop']

      let g:ale_linters = {
        \ 'ruby': [ 'ruby', 'rubocop' ],
        \ 'javascript': [ 'eslint' ],
        \ 'typescript': [ 'tslint' ]
      \ }

      let g:ale_fixers = {
        \ 'javascript': ['prettier'],
        \ 'ruby': ['rubocop'],
        \ 'vue': ['prettier'],
      \ }

      let g:ale_ruby_rubocop_auto_correct_all = 1
      let g:ale_lua_luacheck_options = '--globals vim --max-line-length 150'
    ]])
  end
}
