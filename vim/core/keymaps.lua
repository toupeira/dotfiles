-- vim: foldmethod=marker foldlevel=0

local util = require('util')
local nmap = util.nmap
local vmap = util.vmap
local imap = util.imap
local cmap = util.cmap
local tmap = util.tmap
local nvomap = util.nvomap

-- Mode switching {{{

imap('<C-c>', '<Esc>', 'Leave insert mode')
tmap('<Esc>', '<C-\\><C-n>', 'Leave insert mode')

nmap('<C-c>', function()
  vim.cmd.nohlsearch()
  vim.cmd.echo()

  local _, lualine = pcall(require, 'lualine')
  if lualine then lualine.refresh() end

  local _, jump = pcall(require, 'mini.jump')
  if jump then jump.stop_jumping() end
end, 'Clear search and command line')

-- }}}
-- Window navigation {{{

nmap('<Leader>s', ':split', 'Split horizontally')
nmap('<Leader><Leader>s', ':botright split', 'Split horizontally (full width)')
nmap('<Leader>v', ':vsplit', 'Split vertically')
nmap('<Leader><Leader>v', ':botright vsplit', 'Split vertically (full height)')

nmap('<Leader><Tab>', [[
  empty(getreg('#')) || !buflisted(getreg('#')) ? '<Cmd>bnext<CR>' : '<C-^>'
]], { expr = true, desc = 'Switch to alternate buffer' })

nmap('', '<C-w>=', 'Align windows') -- <C-minus>

nmap('<Leader>c', util.close_window, 'Close current window')

nmap('<Leader>x', function()
  util.close_buffer()
  util.close_tab()
end, 'Close current buffer (keep window)')

nmap('<LocalLeader>q', function() util.toggle_list('c') end, 'Toggle quickfix window')

-- }}}
-- Tab navigation {{{

nmap('H', ':tabprevious', 'Go to previous tab')
nmap('L', ':tabnext', 'Go to next tab')

nmap('<Leader><C-t>', ':tabnew | MiniStarter', 'Open new tab')
nmap('<Leader><C-n>', ':tab split', 'Open current window to new tab')
nmap('<Leader><C-w>', ':tabclose', 'Close current tab')

-- }}}
-- File editing {{{

-- remove default mappings
util.unmap('n', 'Y')
util.unmap('n', '<C-l>')

nmap('<Leader>Q', ':quitall', 'Quit')

nmap('<C-s>', ':write', 'Save current buffer')
nmap('<Leader>w', ':write', 'Save current buffer')
nmap('<Leader>C', function() return ':e ' .. vim.fn.expand('%:p:h') .. '/' end, { expr = true, desc = 'Create file in directory of current buffer' })

vmap('<C-c>', '"+y`]', 'Copy to clipboard')
nmap('<C-v>', '"+gP', 'Paste from clipboard')
vmap('<C-v>', '"+P', 'Paste from clipboard')
imap('<C-v>', '<C-r><C-o>+', 'Paste from clipboard')
cmap('<C-v>', '<C-r><C-o>+', 'Paste from clipboard')
vmap('y', function() return 'ygv' .. vim.fn.mode() end, { expr = true }, 'Yank (keep selection)')

nvomap('+', '"+', 'Clipboard register')
nvomap('X', '"_d', 'Delete to blackhole register')
nmap('XX', '"_dd', 'Delete current line to blackhole register')

vmap('>', '>gv', 'Indent right and reselect')
vmap('<', '<gv', 'Indent left and reselect')

nmap('gp', '`[v`]', 'Select pasted text')
vmap('gs', ':!sort -h', 'Sort selection')
vmap('.', ':normal .', 'Repeat for each line in selection')

imap('<M-o>', '<C-o>o', 'Insert line below')
imap('<M-O>', '<C-o>O', 'Insert line above')

vmap('/', '<C-\\><C-n>`</\\%V', 'Search forward within visual selection')
vmap('?', '<C-\\><C-n>`<?\\%V', 'Search backward within visual selection')

nvomap('*', 'g*', { force = true }, 'Search for current word (loose)')
nvomap('#', 'g#', { force = true }, 'Search backwards for current word (loose)')
nvomap('g*', '*', 'Search for current word (strict)')
nvomap('g#', '#', 'Search backwards for current word (strict)')

nmap('du', ':diffupdate', 'Update diffs')

nmap('<Space>', function()
  local diagnostics = vim.diagnostic.get(0, { lnum = vim.fn.line('.') - 1 })
  if #diagnostics > 0 then
    vim.diagnostic.toggle_current_line()
  else
    return 'za'
  end
end, { expr = true }, 'Toggle fold or show inline diagnostics')

local default_foldlevel = 99999
local original_foldlevel
nmap('<LocalLeader>f', function()
  if vim.o.foldlevel == default_foldlevel then
    vim.o.foldlevel = original_foldlevel or 0
  else
    original_foldlevel = vim.o.foldlevel
    vim.o.foldlevel = default_foldlevel
  end
  util.notify_toggle('Folding:', vim.o.foldlevel ~= default_foldlevel)
end, 'Toggle folding')

local undo_chains = {
  ['<CR>'] = 'new line',
}
for key, desc in pairs(undo_chains) do
  local command
  if key == '<CR>' then
    command = '<C-g>u<C-r>=v:lua.MiniPairs.cr()<CR>'
  else
    command = '<C-g>u' .. key
  end
  imap(key, command, 'Insert ' .. desc .. ' with new undo chain', { silent = true })
end

-- don't open tags with Ctrl-LeftClick
nmap('<C-LeftMouse>', '<nop>')

-- }}}
-- Utilities {{{

util.alias_command({
  ['E']   = 'e',   ['E!']  = 'e!',
  ['Q']   = 'q',   ['Q!']  = 'q!',
  ['QA']  = 'qa',  ['Qa']  = 'qa',  ['qA']  = 'qa',
  ['QA!'] = 'qa!', ['Qa!'] = 'qa!', ['qA!'] = 'qa!',
  ['WQ']  = 'wq',  ['Wq']  = 'wq',  ['wQ']  = 'wq',
  ['WQ!'] = 'wq!', ['Wq!'] = 'wq!', ['wQ!'] = 'wq!',
  ['SET'] = 'set', ['SEt'] = 'set', ['Set'] = 'set',
})

nmap('zS', ':Inspect', 'Inspect highlighting groups')
nmap('zT', ':InspectTree', 'Inspect syntax tree')

nmap('<C-g>', {
  "echomsg expand('%:.')",
  "let @+ = expand('%:.') . ':' . line('.')",
}, 'Show and copy relative file path with number')

nmap('<M-g>', {
  "echomsg expand('%:p')",
  "let @+ = expand('%:p') . ':' . line('.')",
}, 'Show and copy absolute file path with number')

-- }}}
