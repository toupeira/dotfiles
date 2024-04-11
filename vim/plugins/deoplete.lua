return {
  'Shougo/deoplete.nvim',
  event = 'VeryLazy',
  dependencies = {
    { 'Shougo/neco-vim' },
    { 'Shougo/neco-syntax',
      config = function()
        vim.g['necosyntax#max_syntax_lines'] = 500
      end
    },
    { 'wellle/tmux-complete.vim',
      cond = function()
        return os.getenv('TMUX')
      end
    },
  },

  config = function()
    vim.fn['deoplete#enable']()

    vim.cmd([[
      " <Tab> - trigger and accept completion
      inoremap <silent><expr> <Tab>
        \ pumvisible() ? "\<C-y>" :
        \ DeopleteCheckBackSpace() ? "\<Tab>" :
        \ deoplete#complete()

      function! DeopleteCheckBackSpace() abort
        let col = col('.') - 1
        return !col || getline('.')[col - 1]  =~ '\s'
      endfunction

      " <CR> - reject completion and close popup
      inoremap <CR> <C-r>=(pumvisible() ? "<C-e>" : "")<CR><C-g>u<CR>
    ]])
  end
}
