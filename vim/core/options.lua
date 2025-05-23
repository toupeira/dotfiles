local util = require('util')

-- Disable language providers ------------------------------------------

vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_python_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0

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

-- Disable maps from filetype plugins
vim.g.no_plugin_maps = 1

-- Interface -----------------------------------------------------------

if os.getenv('XDG_SESSION_TYPE') ~= 'tty' or os.getenv('SSH_CONNECTION') then
  vim.o.termguicolors = true
  vim.o.pumblend = 10

  if not util.is_headless then
    vim.o.title = true
    vim.o.titlestring = "  %{v:lua.require('util').window_title()} [%{v:lua.require('util').project_path()}]"
  end
end

vim.o.updatetime = 250
vim.opt.shortmess:append {
  a = true,
  A = true,
  c = true,
  C = true,
}

vim.o.cursorline = true
vim.o.cursorlineopt = 'number'

vim.o.splitbelow = true
vim.o.splitright = true
-- vim.o.splitkeep = 'screen'
vim.opt.tabclose:append { 'left', 'uselast' }

vim.o.scrolloff = 5
vim.o.sidescrolloff = 8
vim.opt.virtualedit:append { 'block' }

vim.o.number = true
vim.o.numberwidth = 5
vim.o.relativenumber = true
vim.o.signcolumn = 'yes:2'

vim.opt.completeopt = { 'menu', 'longest', 'preview', 'fuzzy', 'preinsert' }
vim.o.pumheight = 20

vim.o.showmode = false
vim.o.ruler = false
vim.o.report = 0
vim.o.wildmode = 'longest:full,full'

vim.o.mouse = 'ar'
vim.o.mousemodel = 'extend'

-- Get rid of the command window
util.autocmd('CmdWinEnter', 'quit')
vim.o.cmdwinheight = 1
vim.o.cedit = ''

-- History -------------------------------------------------------------

vim.o.undofile = true
vim.o.history = 1000

vim.opt.jumpoptions:remove { 'clean' }

vim.opt.shada:append { "'1000", '\"100' }
vim.opt.shada:remove { "'100", '<50' }

-- Line wrapping and endings -------------------------------------------

vim.opt.fileformats:append { 'mac' }

vim.o.breakindent = true
vim.o.showbreak = ' •• '

-- Indents -------------------------------------------------------------

vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2

vim.o.expandtab = true
vim.o.shiftround = true

-- Searching -----------------------------------------------------------

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.inccommand = 'nosplit'
vim.o.keywordprg = ':Manpage'
vim.o.showmatch = true

vim.opt.iskeyword:append { '-' }
vim.opt.iskeyword:append { '^[' }
vim.opt.iskeyword:append { '^]' }

if vim.fn.executable('rg') then
  vim.o.grepprg = 'rg --vimgrep'
end

-- Concealing ----------------------------------------------------------

vim.o.concealcursor = ''
vim.o.conceallevel = 0

-- Diffs ---------------------------------------------------------------

vim.opt.diffopt:append { 'algorithm:histogram', 'indent-heuristic' }

-- Folds ---------------------------------------------------------------

vim.o.foldmethod = 'indent'
vim.o.foldlevel = 99999

-- Runtime plugin settings ---------------------------------------------

vim.g.markdown_folding = 1
vim.g.markdown_recommended_style = 0
