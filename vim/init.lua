-- use experimental Lua loader
vim.loader.enable()

-- set leader keys
vim.g.mapleader = ','
vim.g.maplocalleader = '\\'

-- set colors for startup errors
vim.api.nvim_set_hl(0, 'ErrorMsg', { fg = '#ffcb6b', bold = true })
vim.api.nvim_set_hl(0, 'Question', { fg = '#c3e88d', bold = true })

-- load configuration
require('core.options')
require('core.keymaps')
require('core.autocmds')
require('core.diagnostics')
require('core.neovide')

-- load plugins
require('core.lazy')
