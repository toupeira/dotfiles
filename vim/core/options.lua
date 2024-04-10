-- Custom filetypes ----------------------------------------------------

vim.filetype.add({
  extension = {
    axlsx = 'ruby',
    prawn = 'ruby',

    bats = 'bash',
    run = 'sh',

    pac = 'javascript',
  },
  filename = {
    Dangerfile = 'ruby',
  },
})

-- Language providers --------------------------------------------------

vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_python_provider = 0
vim.g.loaded_ruby_provider = 0

-- TODO: convert to Lua
vim.cmd([[
  " interface
  set termguicolors
  set updatetime=250
  set mouse=ar
  set mousemodel=extend
  set cursorline
  set cursorlineopt=number

  " buffers
  set scrolloff=5
  set sidescrolloff=8
  set virtualedit+=block

  " windows
  set winheight=3
  set winminheight=3
  set splitbelow
  set splitright
  set splitkeep=screen

  " line numbers and signs
  set number
  set numberwidth=6
  set signcolumn=yes

  " command prompt and status line
  set noshowmode
  set noruler
  set report=0
  set shortmess+=AcC
  set wildmode=longest:full,full

  " get rid of the command window
  autocmd CmdWinEnter * quit
  set cmdwinheight=1
  set cedit=

  " auto-completion
  set completeopt=menuone,noinsert
  set pumblend=10
  set pumheight=15

  " indenting
  set ts=2 sts=2 sw=2
  set expandtab
  set shiftround

  " line endings
  set fileformats+=mac

  " line wrapping
  set breakindent
  let &showbreak = ' •• '

  " searching
  set ignorecase
  set smartcase
  set inccommand=nosplit
  set iskeyword+=-
  set keywordprg=:Manpage

  if executable('rg')
    let &grepprg = "rg --vimgrep"
  endif

  " concealing
  set concealcursor=nvc
  set conceallevel=0

  " diffing
  set diffopt+=algorithm:histogram,indent-heuristic

  " folding
  set foldmethod=indent
  set foldlevel=99999

  " matching
  set showmatch

  " history
  set history=1000
  set shada+='500,\"100
  set shada-='100,<50
  set undofile

  " set a default commentstring
  set commentstring=#\ %s

  " window title
  set title
  let &titlestring = "  %t%{&modified ? ' ●' : ''}%{&readonly ? ' 󰌾 ' : ''} [%{v:lua.require('util').project_path()}]"

  " terminal colors
  " synced from `dconf/com.gexperts.Tilix.ini`
  let g:terminal_color_0  = '#555753'
  let g:terminal_color_1  = '#FF4040'
  let g:terminal_color_2  = '#4D9A05'
  let g:terminal_color_3  = '#C3A000'
  let g:terminal_color_4  = '#417FCF'
  let g:terminal_color_5  = '#8E6199'
  let g:terminal_color_6  = '#05979A'
  let g:terminal_color_7  = '#D3D6CF'

  let g:terminal_color_8  = '#545652'
  let g:terminal_color_9  = '#FF4040'
  let g:terminal_color_10 = '#89E234'
  let g:terminal_color_11 = '#FBE84F'
  let g:terminal_color_12 = '#729ECF'
  let g:terminal_color_13 = '#AC7EA8'
  let g:terminal_color_14 = '#34E2E2'
  let g:terminal_color_15 = '#EDEDEB'
]])
