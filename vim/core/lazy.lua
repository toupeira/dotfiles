local util = require('util')

local LAZY_ROOT = '/slack/dotfiles/packages/lazy'
local LAZY_PATH = LAZY_ROOT .. '/lazy.nvim'

if not vim.uv.fs_stat(LAZY_PATH) then
  vim.fn.system({ 'git', 'clone', '--filter=blob:none', 'https://github.com/folke/lazy.nvim.git', LAZY_PATH, })
end

vim.opt.rtp:prepend(LAZY_PATH)

require('lazy.core.handler.event').mappings.LazyFile = {
  id = 'LazyFile', event = { 'BufReadPre', 'BufNewFile' }
}

util.nmap('<Leader>@', function() require('lazy').home() end, 'Open Lazy')

require('lazy').setup('plugins', {
  root = LAZY_ROOT,
  change_detection = { notify = false },
  concurrency = math.max(4, vim.uv.available_parallelism()) * 8,

  install = {
    missing = not util.is_sudo,
    colorscheme = { 'nordfox', 'habamax' },
  },

  headless = {
    process = false,
    log = true,
    task = false,
  },

  pkg = {
    enabled = false,
  },

  rocks = {
    hererocks = false,
  },

  readme = {
    fles = { 'README.md', 'README.markdown', 'lua/**/README.md' },
  },

  ui = {
    border = 'rounded',
    size = { width = 0.9, height = 0.8 },
  },

  performance = {
    rtp = {
      disabled_plugins = {
        'matchit',
        'matchparen',
        'netrwPlugin',
        'tarPlugin',
        'tohtml',
        'tutor',
        'zipPlugin',
      },
    },
  },
})
