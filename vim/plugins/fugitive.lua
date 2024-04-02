return {
  'tpope/vim-fugitive',
  dependencies = {
    { 'tpope/vim-rhubarb' },
    { 'shumphrey/fugitive-gitlab.vim',
      config = function()
        vim.g.fugitive_gitlab_domains = { 'https://git.panter.ch' }
      end
    }
  },

  config = function()
    vim.cmd([[
      nnoremap <silent> ,gs :G<CR>
      nnoremap <silent><expr> ,gl empty(expand('%')) ? ':Gclog<CR>' : ':Gclog %<CR>'
      vnoremap <silent> ,gl :Gclog<CR>
      noremap  <silent> ,gL :Gclog<CR>
      nnoremap <silent> ,gd :silent Git -p log -p %<CR>
      noremap  <silent> ,gb :Git blame --date human-local<CR>
      noremap  <silent> ,gu :.GBrowse!<CR>
      noremap  <silent> ,gU :.GBrowse<CR>

      autocmd User FugitiveIndex setlocal nobuflisted|nmap <buffer><silent> q gq|nmap <buffer><silent> <space> =|nmap <buffer><silent> <Tab> =
      autocmd User FugitiveObject nmap <buffer><silent> q :cclose<CR>:bd<CR>
      autocmd User FugitivePager if maparg('gq', 'n')!=''|nmap <buffer><silent> q gq|endif
    ]])
  end
}
