local util = require('util')
local autocmd = util.autocmd

-- Filetype settings ---------------------------------------------------

local filetypes = {
  ['crontab']   = { 'nowritebackup' },
  ['css,scss']  = { 'iskeyword+=%' },
  ['dosini']    = { 'foldmethod=syntax' },
  ['gdscript']  = { 'expandtab' },
  ['gitconfig'] = { 'commentstring=#\\ %s' },
  ['iss']       = { 'commentstring=;\\ %s' },
  ['lua']       = { 'path+=./lua', 'keywordprg=:help' },
  ['make']      = { 'noexpandtab' },
  ['ruby']      = { 'iskeyword+=?,!' },

  ['gitcommit,gitrebase,NeogitCommitMessage'] = {
    'colorcolumn=50,72',
  },

  ['help'] = {
    'buflisted',
  },

  ['help,man,qf'] = {
    'winhighlight=Normal:NormalFloat',
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
}

for filetype, settings in pairs(filetypes) do
  autocmd('FileType', filetype, 'setlocal ' .. util.join(settings, ' '))
end

-- Helpers -------------------------------------------------------------

-- Resize quickfix windows
autocmd('FileType', 'qf', function()
  util.resize_window({ max = 10 })
end)

-- Enter insert mode when committing
autocmd('FileType', 'gitcommit', function()
  if vim.fn.getline(1) == '' then
    vim.cmd.normal('O')
    vim.cmd.startinsert()
  end
end)

-- Setup terminals
autocmd('TermOpen', function()
  vim.wo.winhighlight = 'Normal:NormalFloat'
end)

--- Automatically enter insert mode for terminals
autocmd({ 'BufWinEnter', 'WinEnter' }, 'term://*', 'startinsert!')

-- Git helpers
local git_command = function(action, key, command, check)
  local desc = action .. ' changes interactively'
  util.command('G' .. action, function()
    if vim.fn.system(check) ~= '' then
      vim.cmd.terminal(command)
    else
      util.notify('Git:', {
        annote = 'No changes to ' .. action:lower() .. '.',
        level = 'WARN'
      })
    end
  end, desc)

  util.nmap(key, ':G' .. action, desc)
end

git_command('Stage', '<Leader>gA', 'git add -p', 'git unstaged')
git_command('Unstage', '<Leader>gU', 'git reset HEAD -p', 'git staged')
git_command('Discard', '<Leader>gD', 'git checkout -p', 'git unstaged')

-- Auto-close Git commands on success or cancel
util.autocmd('TermClose', 'term://*:git *', function()
  if vim.v.event.status == 0 or vim.v.event.status == 130 then
    util.close_buffer()
    util.close_window()
  end
end)

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

-- Add some mappings for `:Man` command from `runtime/ftplugin/man.vim`
autocmd('FileType', 'man', function()
  util.nmap('gO', require('man').show_toc, 'Show table of contents', { force = true })
  util.nmap('q', ':lclose<CR><C-w>q', { buffer = true })
end)

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
