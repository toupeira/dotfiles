-- vim: foldmethod=marker foldlevel=0

local util = require('util')
local autocmd = util.autocmd

-- Filetype settings {{{

local filetypes = {
  ['crontab']   = { 'nowritebackup' },
  ['css,scss']  = { 'iskeyword+=%' },
  ['dosini']    = { 'foldmethod=syntax' },
  ['gdscript']  = { 'expandtab' },
  ['gitconfig'] = { 'commentstring=#\\ %s' },
  ['iss']       = { 'commentstring=;\\ %s' },
  ['lua']       = { 'foldlevel=1', 'path+=./lua', 'keywordprg=:help' },
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

  ['markdown,codecompanion'] = {
    'conceallevel=2',
    'suffixesadd+=.md',
    'foldlevel=2',
    -- automatically continue lists and blockquotes
    'comments=b:*,b:-,b:+,n:>',
    'formatoptions+=r',
  },
}

for filetype, settings in pairs(filetypes) do
  autocmd('FileType', filetype, 'setlocal ' .. util.join(settings, ' '))
end

-- Enable Treesitter folding if available
util.autocmd('FileType', function()
  if not vim.treesitter.highlighter.active[vim.api.nvim_get_current_buf()] then
    return
  end

  vim.wo[0][0].foldmethod = 'expr'
  if vim.bo.filetype == 'markdown' then
    -- Use default folding from ftplugin for Markdown
    vim.wo[0][0].foldexpr = 'MarkdownFold()'
  else
    vim.wo[0][0].foldexpr = 'v:lua.vim.treesitter.foldexpr()'
  end
end)

-- }}}
-- Helpers {{{

-- Resize quickfix windows
autocmd('FileType', 'qf', function()
  util.resize_window({
    max = math.floor(util.clamp(vim.o.lines / 3, 1, 10)),
  })
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
  vim.wo[0][0].winhighlight = 'Normal:NormalFloat'
end)

-- Automatically enter insert mode for terminals
autocmd({ 'BufWinEnter', 'WinEnter' }, 'term://*', 'startinsert!')

-- Grep wrapper
util.command('Grep', function(info)
  vim.cmd('silent grep! ' .. info.args)
  if #vim.fn.getqflist() > 0 then
    vim.cmd.copen()
  else
    util.echo('No results for "' .. info.args .. '"', 'WarningMsg')
  end
end, { nargs = '+', complete = 'file' })

util.alias_command({ gr = 'Grep', rg = 'Grep' })

-- Git helpers
local git_command = function(action, key, command, check)
  local desc = action .. ' changes interactively'
  util.command('G' .. action, function()
    if vim.fn.system(check) ~= '' then
      require('snacks.terminal').open(command, {
        win = { height = 0.5 },
      })
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

util.command('Edit', function(info)
  local output = vim.system(
    { 'sh', '-c', 'git-edit -n ' .. info.args },
    { text = true }
  ):wait()

  local message = output.stderr
    :gsub('\27.-m', '')
    :gsub('\n', '')

  if output.code == 0 then
    local files = vim.split(output.stdout:gsub('\n$', ''), '\n')
    if #files == 0 or files[1] == '' then
      util.echo('  No ' .. info.args .. ' files', 'WarningMsg')
      return
    end

    util.echo(message, 'MoreMsg')
    local old_buf = vim.fn.bufnr()

    for file in vim.iter(files) do
      vim.cmd.badd(file)
    end

    vim.cmd('silent drop ' .. files[1])

    if vim.api.nvim_buf_is_valid(old_buf) and vim.bo[old_buf].buftype == '' and vim.fn.bufname(old_buf) == '' and not vim.bo[old_buf].modified then
      vim.cmd('silent bdelete ' .. old_buf)
    end
  else
    util.echo(message, 'WarningMsg')
  end
end, {
  nargs = '*',
  complete = function (pattern)
    return vim.iter({
      vim.iter({
        '--modified',
        '--staged',
        '--unstaged',
        '--last',
      }):filter(function(candidate)
        return candidate:match('^' .. pattern:gsub('-', '%%-'))
      end):totable(),
      vim.fn.getcompletion(pattern, 'file')
    }):flatten():totable()
  end
}, 'Edit Git files')

util.alias_command({ E = 'Edit' })

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
autocmd({ 'FocusGained', 'TermLeave', 'VimResume' }, function()
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

-- }}}
