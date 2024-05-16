local util = require('util')
local autocmd = util.autocmd

-- Filetype settings ---------------------------------------------------

vim.cmd([[
  autocmd FileType crontab             setlocal nowritebackup
  autocmd FileType css,scss            setlocal iskeyword+=%
  autocmd FileType dosini              setlocal commentstring=#\ %s
  autocmd FileType gdscript            setlocal expandtab
  autocmd FileType gitcommit,gitrebase setlocal colorcolumn=50,72
  autocmd FileType help                setlocal buflisted
  autocmd FileType lua                 setlocal path+=./lua keywordprg=:Help
  autocmd FileType make                setlocal noexpandtab
  autocmd FileType qf                  setlocal nobuflisted
  autocmd FileType ruby                setlocal iskeyword+=?,! indentkeys-=.
  autocmd FileType text,markdown,mail  setlocal linebreak suffixesadd+=.md
  autocmd FileType vim                 setlocal keywordprg=:Help foldmethod=marker foldlevel=0
]])

-- Helpers -------------------------------------------------------------

-- Setup quickfix windows
autocmd('FileType', 'qf', function()
  vim.wo.wrap = false
  util.resize_window({ max = 5 })
end)

-- Setup terminals
autocmd('TermOpen', function()
  vim.wo.number = false
  vim.wo.relativenumber = false
  vim.wo.signcolumn = 'no'
  vim.wo.winhighlight = 'Normal:TermCursorNC'
end)

-- Close terminals on successful exit
autocmd('TermClose', function()
  if vim.v.event.status and vim.bo.filetype ~= 'fzf' then
    vim.cmd.bdelete()
  end
end)

-- Automatically enter/leave insert mode for terminals
autocmd({ 'BufWinEnter', 'WinEnter' }, 'term://*', 'startinsert!')
autocmd({ 'BufWinLeave', 'WinLeave' }, 'term://*', 'stopinsert')

-- Enter insert mode when committing
autocmd('FileType', 'gitcommit', function()
  if vim.fn.getline(1) == '' then
    vim.cmd.normal('O')
    vim.cmd.startinsert()
  end
end)

-- Autocommands adapted from
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua

-- Go to last loc when opening a buffer
-- TODO: fix `mini.misc.setup_restore_cursor` to use `BufWinEnter`
autocmd('BufWinEnter', function(event)
  local exclude = { 'gitcommit', 'gitrebase' }
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
end)

-- Close some filetypes with <q>
autocmd('FileType', {
  'bufferize',
  'checkhealth',
  'fugitive',
  'help',
  'man',
  'notify',
  'qf',
  'query',
  'startuptime',
},
  function(event)
    util.nmap('q', {
      'bdelete',
      'lua require("util").close_tab()'
    }, { force = true, buffer = event.buf })
  end
)

-- Check if we need to reload the file when it changed
autocmd({ 'FocusGained', 'TermClose', 'TermLeave' }, function()
  if vim.o.buftype ~= 'nofile' then
    vim.cmd.checktime()
  end
end)

-- Auto create dir when saving a file, in case some
-- intermediate directory does not exist
autocmd('BufWritePre', function(event)
  if event.match:match('^%w%w+:[\\/][\\/]') then
    return
  end
  local file = vim.uv.fs_realpath(event.match) or event.match
  vim.fn.mkdir(vim.fn.fnamemodify(file, ':p:h'), 'p')
end)
