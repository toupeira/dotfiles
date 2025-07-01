local util = require('util')
local nmap = util.nmap
local vmap = util.vmap

return {
  'tpope/vim-fugitive',

  init = function()
    nmap('<Leader>gl', "&modifiable ? ':Gclog %<CR>' : ':Gclog<CR>'", { expr = true, silent = true },
      'Open Git log for current file')
    vmap('<Leader>gl', ':Gclog', 'Open Git log for selection')
    nmap('<Leader>gL', ':Gclog', 'Open Git log for repository')
    nmap('<Leader>gd', ':Git log -p %', 'Open Git diff for current file')

    nmap('<Leader>gF', ':Git pull -v', 'Pull current Git branch')
    nmap('<Leader>gP', ':Git push -v', 'Push current Git branch')
  end,

  config = function()
    util.autocmd('User', 'FugitiveIndex', function()
      vim.bo.buflisted = false
      nmap('<Space>', '=', { remap = true, buffer = true, force = true })
      nmap('<Tab>', '=', { remap = true, buffer = true, force = true })
    end)

    util.autocmd('User', { 'FugitiveObject', 'FugitivePager' }, function()
      nmap('q', { 'cclose', 'bwipeout' }, { buffer = true })
    end)
  end,
}
