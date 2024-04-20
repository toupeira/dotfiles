local util = require('util')
local nmap = util.nmap
local vmap = util.vmap
local nvomap = util.nvomap

return {
  'tpope/vim-fugitive',
  event = 'VeryLazy',
  dependencies = {
    { 'tpope/vim-rhubarb', event = 'LazyFile' },
    { 'shumphrey/fugitive-gitlab.vim',
      event = 'LazyFile',
      config = function()
        vim.g.fugitive_gitlab_domains = { 'https://git.panter.ch' }
      end
    }
  },

  init = function()
    nmap('<Leader>gl', "&modifiable ? ':Gclog %<CR>' : ':Gclog<CR>'", { expr = true },
      'Open Git log for current file')
    vmap('<Leader>gl', ':Gclog', 'Open Git log for selection')
    nmap('<Leader>gL', ':Gclog', 'Open Git log for repository')

    nmap('<Leader>gd', ':Git -p log -p %', 'Open Git diff for current file')
    nvomap('<Leader>gb', ':Git blame --date human-local', 'Open Git blame for current file')

    nvomap('<Leader>gu', "':' . (mode() == 'n' && '.') . 'GBrowse!<CR>'", { expr = true },
      'Copy Git URL to current location')
    nvomap('<Leader>gU', "':' . (mode() == 'n' && '.') . 'GBrowse<CR>'", { expr = true },
      'Browse Git URL to current location')
  end,

  config = function()
    vim.cmd([[
      autocmd User FugitiveIndex setlocal nobuflisted|nmap <buffer><silent> q gq|nmap <buffer><silent> <space> =|nmap <buffer><silent> <Tab> =
      autocmd User FugitiveObject,FugitivePager nmap <buffer><silent> q :cclose<CR>:bd<CR>
    ]])
  end
}
