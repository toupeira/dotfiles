local util = require('util')

return { 'sindrets/diffview.nvim',
  cmd = { 'DiffviewOpen', 'DiffviewFileHistory', },
  keys = {
    { '<Leader>gD', '<Cmd>DiffviewOpen<CR>', desc = 'Open Diffview' },
  },

  opts = {
    view = {
      default = { layout = 'diff2_vertical' },
    },
  },

  init = function()
    util.autocmd('FileType', { 'DiffviewFiles', 'DiffviewFileHistory' }, function()
      util.nmap({ 'q', 'gq' }, ':DiffviewClose', { buffer = true })
    end)
  end
}
