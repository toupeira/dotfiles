local util = require('util')

local map = util.map
local nmap = util.nmap
local vmap = util.vmap
local imap = util.imap
local cmap = util.cmap
local tmap = util.tmap
local nvomap = util.nvomap

local expand = vim.fn.expand

-- Leader keys

vim.g.mapleader = ','
vim.g.maplocalleader = '\\'

-- Mode switching

imap('<C-c>', '<Esc>', 'Leave insert mode')
nmap('<C-c>', { 'nohlsearch', 'echo', 'redraw!' }, 'Clear search and command line')
tmap('<Esc>', '<C-\\><C-n>', 'Leave insert mode')

-- Window navigation

nmap('<Leader>s', ':split', 'Split horizontally')
nmap('<Leader>S', ':botright split', 'Split horizontally (full width)')
nmap('<Leader>v', ':vsplit', 'Split vertically')
nmap('<Leader>V', ':botright vsplit', 'Split vertically (full height)')

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

nmap('<Leader>x', { 'lclose', 'BufDel' }, 'Close current buffer (keep window)')

nmap('<Leader>q', function() util.toggle_list('c') end, 'Toggle quickfix window')

-- Tab navigation

nmap('H', ':tabprevious', 'Go to previous tab')
nmap('L', ':tabnext', 'Go to next tab')
nmap('<Leader>N', ':tabprevious', 'Go to previous tab')
nmap('<Leader>P', ':tabnext', 'Go to next tab')

nmap('<Leader>T', ':tabnew', 'Open new tab')
nmap('<Leader>X', ':tabclose', 'Close current tab')

-- File editing

nmap('<Leader>w', ':write', 'Save current buffer')

nmap('<Leader>C', function() return ':e ' .. expand('%:p:h') .. '/' end, { expr = true, desc = 'Create file in directory of current buffer' })

-- use <C-q> as replacement for <C-v>
map({ 'n', 'v', 'o', 'i', 'c' }, '<C-q>', '<C-v>', 'Insert raw character')

vmap('<C-c>', '"+y', 'Copy to clipboard')
vmap('<C-x>', '"+d', 'Cut to clipboard')

nmap('<C-v>', '"+gP', 'Paste from clipboard')
vmap('<C-v>', '"+P', 'Paste from clipboard')
imap('<C-v>', '<C-r><C-o>+', 'Paste from clipboard')
cmap('<C-v>', '<C-r><C-o>+', 'Paste from clipboard')

nvomap('+', '"+', 'Clipboard register')
nvomap('X', '"_d', 'Delete to blackhole register')
nmap('XX', '"_dd', 'Delete current line to blackhole register')

vmap('>', '>gv', 'Indent right and reselect')
vmap('<', '<gv', 'Indent left and reselect')
nmap('vp', '`[v`]', 'Select pasted text')
vmap('gs', ':!sort -h', 'Sort selection')
vmap('.', ':normal .', 'Repeat for each line in selection')

imap('<M-o>', '<C-o>o', 'Insert line below')
imap('<M-O>', '<C-o>O', 'Insert line above')

nvomap('*', 'g*', 'Search for current word (loose)')
nvomap('#', 'g#', 'Search backwards for current word (loose)')
nvomap('g*', '*', 'Search for current word (strict)')
nvomap('g#', '#', 'Search backwards for current word (strict)')

nmap('<Space>', 'za', 'Toggle fold')
nmap('du', ':diffupdate', 'Update diffs')

-- Utilities

nmap('<F1>', ':tab help', 'Open help in a tab')

util.alias_cmd({
  ['h'] = 'tab help',
  ['H'] = 'tab help',
  ['help'] = 'tab help',

  ['E']   = 'e', ['E!']  = 'e!',
  ['Q']   = 'q', ['Q!']  = 'q!',
  ['QA']  = 'qa', ['Qa']  = 'qa', ['qA']  = 'qa',
  ['QA!'] = 'qa!', ['Qa!'] = 'qa!', ['qA!'] = 'qa!',
  ['WQ']  = 'wq', ['Wq']  = 'wq', ['wQ']  = 'wq',
  ['WQ!'] = 'wq!', ['Wq!'] = 'wq!', ['wQ!'] = 'wq!',

  ['DD']  = 'Delete', ['DD!'] = 'Delete!',
})

nmap('zS', ':Inspect', 'Inspect highlighting groups')
nmap('<Leader>!', function() require('lazy').home() end, 'Open Lazy plugin manager')

nmap('<C-g>', {
  "echomsg expand('%:.')",
  "let @+ = expand('%:.') . ':' . line('.')",
}, 'Copy file path with number')

-- TODO: convert to Lua
vim.cmd([[
  " don't map Y to y$
  if maparg('Y', 'n') != ''
    nunmap Y
  endif

  " don't open tags with Ctrl-LeftClick
  noremap <C-LeftMouse> <nop>

  " break undo chain on new lines
  inoremap <CR> <C-g>u<CR>

  " <y> - keep cursor position when yanking in visual mode
  xnoremap <silent><expr> y "ygv" . mode()

  " switch to alternate buffer
  nnoremap <silent><expr> ,<Tab> empty(getreg('#')) \|\| !buflisted(getreg('#')) ? ':bnext<CR>' : '<C-^>'

  " navigate windows/tmux panes with Ctrl+jkhl
  function! TmuxNavigate(cmd)
    let nr = winnr()

    if nvim_win_get_config(nvim_get_current_win()).relative == ''
      execute('wincmd ' . a:cmd)
    endif

    if (empty($TMUX))
      return
    elseif (nr != winnr())
      return
    endif

    let pane = str2nr(system('tmux display -p "#{pane_index}"'))
    let panes = str2nr(system('tmux display -p "#{window_panes}"'))

    if (((a:cmd == 'k' || a:cmd == 'h') && nr == 1 && pane == 1) ||
          \ ((a:cmd == 'j' || a:cmd == 'l') && nr == winnr('$') && pane == panes))
      " silent call system('tmux resize-pane -Z')
    else
      let arg = tr(a:cmd, 'hjkl', 'LDUR')
      silent call system('tmux select-pane -' . arg)
    endif
  endfunction

  nnoremap <silent> <c-h> :call TmuxNavigate('h')<CR>
  nnoremap <silent> <c-j> :call TmuxNavigate('j')<CR>
  nnoremap <silent> <c-k> :call TmuxNavigate('k')<CR>
  nnoremap <silent> <c-l> :call TmuxNavigate('l')<CR>

  tnoremap <silent> <c-h> <C-\><C-n>:call TmuxNavigate('h')<CR>
  tnoremap <silent> <c-j> <C-\><C-n>:call TmuxNavigate('j')<CR>
  tnoremap <silent> <c-k> <C-\><C-n>:call TmuxNavigate('k')<CR>
  tnoremap <silent> <c-l> <C-\><C-n>:call TmuxNavigate('l')<CR>
]])
