local util = require('util')

local nmap = util.nmap
local vmap = util.vmap
local imap = util.imap
local cmap = util.cmap
local tmap = util.tmap
local nvomap = util.nvomap

local expand = vim.fn.expand

-- Mode switching ------------------------------------------------------

imap('<C-c>', '<Esc>', 'Leave insert mode')
nmap('<C-c>', { 'nohlsearch', 'echo', 'lua= require("lualine").refresh()' }, 'Clear search and command line')
tmap('<Esc>', '<C-\\><C-n>', 'Leave insert mode')

-- Window navigation ---------------------------------------------------

nmap('<Leader>s', ':split', 'Split horizontally')
nmap('<Leader>S', ':botright split', 'Split horizontally (full width)')
nmap('<Leader>v', ':vsplit', 'Split vertically')
nmap('<Leader>V', ':botright vsplit', 'Split vertically (full height)')

nmap('<Leader><Tab>', [[
  empty(getreg('#')) || !buflisted(getreg('#')) ? '<Cmd>bnext<CR>' : '<C-^>'
]], { expr = true, desc = 'Switch to alternate buffer' })

nmap('', '<C-w>=', 'Align windows') -- <C-minus>

nmap('<Leader>c', function()
  -- don't close window if it's the last normal one
  if vim.bo.buftype ~= 'quickfix' then
    local windows = #vim.tbl_filter(
      function(win) return vim.fn.win_gettype(win) == '' end,
      vim.fn.range(1, vim.fn.winnr('$'))
    )

    if windows <= 1 then return end
  end

  pcall(vim.cmd.close)
end, 'Close current window')

nmap('<Leader>q', function() util.toggle_list('c') end, 'Toggle quickfix window')

-- Tab navigation ------------------------------------------------------

nmap('H', ':tabprevious', 'Go to previous tab')
nmap('L', ':tabnext', 'Go to next tab')

nmap('<Leader>N', ':tabnew', 'Open new tab')
nmap('<Leader>X', ':tabclose', 'Close current tab')

-- File editing --------------------------------------------------------

nmap('<Leader>w', ':write', 'Save current buffer')
nmap('<Leader>C', function() return ':e ' .. expand('%:p:h') .. '/' end, { expr = true, desc = 'Create file in directory of current buffer' })

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
nmap('vp', '`[v`]', 'Select pasted text')
vmap('gs', ':!sort -h', 'Sort selection')
vmap('.', ':normal .', 'Repeat for each line in selection')

imap('<CR>', '<C-g>u<CR>', 'Break undo chain and insert new line')
imap('<M-o>', '<C-o>o', 'Insert line below')
imap('<M-O>', '<C-o>O', 'Insert line above')

nvomap('*', 'g*', { force = true }, 'Search for current word (loose)')
nvomap('#', 'g#', { force = true }, 'Search backwards for current word (loose)')
nvomap('g*', '*', 'Search for current word (strict)')
nvomap('g#', '#', 'Search backwards for current word (strict)')

nmap('<Space>', 'za', 'Toggle fold')
nmap('du', ':diffupdate', 'Update diffs')

-- don't map Y to y$
util.unmap('n', 'Y')

-- don't open tags with Ctrl-LeftClick
nmap('<C-LeftMouse>', '<nop>')

-- Utilities -----------------------------------------------------------

util.command('Help', '$tab help <args>', {
  nargs = '*',
  complete = 'help',
})

nmap('<F1>', ':Help', 'Open help in a new tab')

-- override `:Man` to open in a tab
-- see /usr/share/nvim/runtime/plugin/man.lua
util.command('Manpage', '$tab Man <args>', {
  bang = true,
  bar = true,
  range = true,
  addr = 'other',
  nargs = '*',
  complete = function(...)
    return require('man').man_complete(...)
  end,
})

util.alias_cmd({
  help = 'Help',
  hel  = 'Help',
  he   = 'Help',
  h    = 'Help',
  H    = 'Help',
  man  = 'Manpage',
  Man  = 'Manpage',

  ['E']   = 'e',   ['E!']  = 'e!',
  ['Q']   = 'q',   ['Q!']  = 'q!',
  ['QA']  = 'qa',  ['Qa']  = 'qa',  ['qA']  = 'qa',
  ['QA!'] = 'qa!', ['Qa!'] = 'qa!', ['qA!'] = 'qa!',
  ['WQ']  = 'wq',  ['Wq']  = 'wq',  ['wQ']  = 'wq',
  ['WQ!'] = 'wq!', ['Wq!'] = 'wq!', ['wQ!'] = 'wq!',

  ['DD']  = 'Delete', ['DD!'] = 'Delete!',
})

nmap('zS', ':Inspect', 'Inspect highlighting groups')

nmap('<C-g>', {
  "echomsg expand('%:.')",
  "let @+ = expand('%:.') . ':' . line('.')",
}, 'Show and copy relative file path with number')

nmap('<M-g>', {
  "echomsg expand('%:p')",
  "let @+ = expand('%:p') . ':' . line('.')",
}, 'Show and copy absolute file path with number')
