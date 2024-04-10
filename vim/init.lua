-- set colors for startup errors
vim.cmd('hi! ErrorMsg guifg=#ffcb6b gui=bold guibg=NONE')
vim.cmd('hi! Question guifg=#C3E88D gui=bold')

require('core.options')
require('core.autocmds')
require('core.keymaps')
require('core.diagnostics')
require('core.lazy')
