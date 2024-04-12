local LAZY_ROOT = '/etc/dotfiles/packages/lazy'
local LAZY_PATH = LAZY_ROOT .. '/lazy.nvim'

vim.opt.rtp:prepend(LAZY_PATH)

if not (vim.uv or vim.loop).fs_stat(LAZY_PATH) then
  vim.fn.system({ 'git', 'clone', '--filter=blob:none', 'https://github.com/folke/lazy.nvim.git', LAZY_PATH, })
end

require('lazy').setup('plugins', {
  root = LAZY_ROOT,
  install = { missing = true, colorscheme = { 'nordfox', 'habamax' }},
  change_detection = { notify = false },

  ui = {
    border = 'rounded',
    size = { width = 0.9, height = 0.8 },
  },

  readme = {
    fles = { 'README.md', 'README.markdown', 'lua/**/README.md' },
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
