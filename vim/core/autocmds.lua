local util = require('util')

local autocmd = util.autocmd
local augroup = util.augroup

-- TODO: convert to Lua
vim.cmd([[
  " filetype settings
  autocmd FileType crontab setlocal nowritebackup
  autocmd FileType css,scss setlocal iskeyword+=%
  autocmd FileType dosini setlocal commentstring=#\ %s
  autocmd FileType gdscript setlocal expandtab
  autocmd FileType gitcommit,gitrebase setlocal colorcolumn=50,72
  autocmd FileType lua setlocal path+=./lua keywordprg=:help
  autocmd FileType make setlocal noexpandtab
  autocmd FileType qf setlocal nobuflisted
  autocmd FileType ruby setlocal iskeyword+=?,!
  autocmd FileType text,markdown,mail setlocal linebreak suffixesadd+=.md
  autocmd FileType vim setlocal foldmethod=marker foldlevel=0

  " setup quickfix windows
  autocmd FileType qf setlocal nowrap
  autocmd FileType qf lua require('util').resize_window({ max = 5 })

  " terminals
  autocmd TermOpen * setlocal nonumber norelativenumber signcolumn=no
  autocmd TermClose * if !v:event.status && &ft != 'fzf' | bd | endif
  autocmd BufWinEnter,WinEnter term://* startinsert!
  autocmd BufWinLeave,WinLeave term://* stopinsert
]])

-- Start insert mode when committing
autocmd('FileType', {
  group = augroup('commit_insert'),
  pattern = 'gitcommit',
  callback = function()
    if vim.fn.getline(1) == '' then
      vim.cmd.normal('O')
      vim.cmd.startinsert()
    end
  end
})

-- Adapted from
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua

-- Go to last loc when opening a buffer
-- TODO: fix `mini.misc.setup_restore_cursor` to use `BufWinEnter`
autocmd('BufWinEnter', {
  group = augroup('last_loc'),
  callback = function(event)
    local exclude = { 'gitcommit' }
    local buf = event.buf
    if vim.tbl_contains(exclude, vim.bo[buf].filetype) or vim.b[buf].last_loc then
      return
    end
    vim.b[buf].last_loc = true
    local mark = vim.api.nvim_buf_get_mark(buf, '"')
    local lcount = vim.api.nvim_buf_line_count(buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
      vim.cmd('normal! zvzz')
    end
  end
})

-- Close some filetypes with <q>
autocmd('FileType', {
  group = augroup('close_with_q'),
  pattern = {
    'checkhealth',
    'fugitive',
    'help',
    'lspinfo',
    'notify',
    'qf',
    'query',
    'startuptime',
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    util.nmap('q', ':bdelete', { buffer = event.buf })
  end
})

-- Check if we need to reload the file when it changed
autocmd({ 'FocusGained', 'TermClose', 'TermLeave' }, {
  group = augroup('checktime'),
  callback = function()
    if vim.o.buftype ~= 'nofile' then
      vim.cmd('checktime')
    end
  end
})

-- Auto create dir when saving a file, in case some intermediate directory does not exist
autocmd('BufWritePre', {
  group = augroup('auto_create_dir'),
  callback = function(event)
    if event.match:match('^%w%w+:[\\/][\\/]') then
      return
    end
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ':p:h'), 'p')
  end
})
