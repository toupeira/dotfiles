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
  autocmd FileType lua                 setlocal path+=./lua iskeyword-=: keywordprg=:Help
  autocmd FileType make                setlocal noexpandtab
  autocmd FileType markdown            setlocal foldmethod=indent
  autocmd FileType qf                  setlocal nobuflisted
  autocmd FileType ruby                setlocal iskeyword+=?,!
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

-- Automatically enter/leave insert mode for terminals
autocmd({ 'BufWinEnter', 'WinEnter' }, 'term://*', 'startinsert!')
autocmd({ 'BufWinLeave', 'WinLeave' }, 'term://*', 'stopinsert')

-- Auto-close certain terminal commands
util.autocmd('TermClose', 'term://*:{git,dotfiles} *', function()
  util.close_buffer()
  util.close_window()
end)

-- Enter insert mode when committing
autocmd('FileType', 'gitcommit', function()
  if vim.fn.getline(1) == '' then
    vim.cmd.normal('O')
    vim.cmd.startinsert()
  end
end)

-- Git helpers
local git_command = function(action, key, command, check)
  local desc = action .. ' changes interactively'
  util.command('G' .. action, function()
    if vim.fn.system(check) ~= '' then
      vim.cmd.terminal(command)
    else
      util.echo('No changes to ' .. action:lower() .. '.', 'ModeMsg')
    end
  end, desc)

  util.nmap(key, ':G' .. action, desc)
end

git_command('Stage', '<Leader>gA', 'git add -p', 'git unstaged')
git_command('Unstage', '<Leader>gU', 'git reset HEAD -p', 'git staged')
git_command('Discard', '<Leader>gD', 'git checkout -p', 'git unstaged')

-- Dotfiles helpers
util.command('Dotfiles', 'terminal dotfiles <args>', { nargs = '*' })
util.alias_command({ DT = 'Dotfiles' })

-- Autocommands adapted from
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua

-- Go to last loc when opening a buffer
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
  'gitsigns-blame',
  'help',
  'man',
  'notify',
  'qf',
  'query',
  'startuptime',
},
  function(event)
    util.nmap('q', {
      'bwipeout',
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
