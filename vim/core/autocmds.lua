local util = require('util')
local autocmd = util.autocmd

-- Filetype settings ---------------------------------------------------

local filetypes = {
  ['crontab']  = { 'nowritebackup' },
  ['css,scss'] = { 'iskeyword+=%' },
  ['dosini']   = { 'commentstring=#\\ %s', 'foldmethod=syntax' },
  ['gdscript'] = { 'expandtab' },
  ['help']     = { 'buflisted' },
  ['iss']      = { 'commentstring=;\\ %s' },
  ['lua']      = { 'path+=./lua', 'keywordprg=:help' },
  ['make']     = { 'noexpandtab' },
  ['qf']       = { 'nobuflisted' },
  ['ruby']     = { 'iskeyword+=?,!' },

  ['gitcommit,gitrebase,NeogitCommitMessage'] = {
    'colorcolumn=50,72',
  },

  ['text,mail,markdown'] = {
    'linebreak',
  },

  ['markdown'] = {
    'conceallevel=2',
    'suffixesadd+=.md',
    'foldlevel=2',
    -- reset after obsidian.nvim
    'foldexpr=MarkdownFold()',
    -- automatically continue lists and blockquotes
    'comments=b:*,b:-,b:+,n:>',
    'formatoptions+=r',
  },

  ['vim'] = {
    'foldmethod=marker',
    'foldlevel=0'
  },
}

for filetype, settings in pairs(filetypes) do
  autocmd('FileType', filetype, 'setlocal ' .. table.concat(settings, ' '))
end

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
  vim.wo.winhighlight = 'Normal:NormalFloat'
end)

-- Automatically enter/leave insert mode for terminals
autocmd({ 'BufWinEnter', 'WinEnter' }, 'term://*', 'startinsert!')
autocmd({ 'BufWinLeave', 'WinLeave' }, 'term://*', 'stopinsert')

-- Auto-close certain terminal commands
util.autocmd('TermClose', 'term://*:{git,dotfiles,glow} *', function()
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

util.command('Preview', 'vsplit | terminal glow %')

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
  'neo-tree-popup',
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
