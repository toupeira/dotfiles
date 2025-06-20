local util = require('util')
local nmap = util.nmap
local vmap = util.vmap
local nvomap = util.nvomap

return {
  'tpope/vim-fugitive',
  dependencies = { -- {{{
    { 'tpope/vim-rhubarb',
      dependencies = { 'fugitive-gitlab.vim' },
    },
    { 'shumphrey/fugitive-gitlab.vim',
      config = function()
        vim.g.fugitive_browse_handlers = {
          function(...)
            local remote = vim.fn.FugitiveRemote()
            if not remote then return end

            if remote.host:match('^git%.') or remote.host:match('^gitlab%.') then
              vim.g.fugitive_gitlab_domains = { 'https://' .. remote.host }
            end

            return vim.fn['gitlab#fugitive#handler'](...)
          end
        }
      end,
    }
  }, -- }}}

  init = function()
    vmap('<Leader>gl', ':Gclog', 'Open Git log for selection')
    nmap('<Leader>gd', ':Git log -p %', 'Open Git diff for current file')

    nmap('<Leader>gF', ':Git pull -v', 'Pull current Git branch')
    nmap('<Leader>gP', ':Git push -v', 'Push current Git branch')

    nvomap('<Leader>gx', "':' . (mode() == 'n' ? '.' : '') . 'GBrowse!<CR>'", { expr = true, silent = true },
      'Copy Git URL to current location')
    nvomap('<Leader>gX', "':' . (mode() == 'n' ? '.' : '') . 'GBrowse<CR>'", { expr = true, silent = true },
      'Browse Git URL to current location')
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
