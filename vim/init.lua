-- use experimental Lua loader
vim.loader.enable()

-- set leader keys
vim.g.mapleader = ','
vim.g.maplocalleader = '\\'

-- set colors for startup errors
vim.cmd('hi! ErrorMsg guifg=#ffcb6b gui=bold guibg=NONE')
vim.cmd('hi! Question guifg=#C3E88D gui=bold')

-- load configuration
require('core.lazy')
require('core.options')
require('core.autocmds')
require('core.keymaps')
require('core.diagnostics')
