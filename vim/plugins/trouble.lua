local util = require('util')

return {
  'folke/trouble.nvim',
  event = 'VeryLazy',
  branch = 'dev',

  keys = {
    {
      '<Leader>e',
      function()
        local trouble = require('trouble')
        if trouble.is_open() then
          trouble.close()
        elseif #vim.diagnostic.get(0) > 0 then
          trouble.toggle({ mode = 'diagnostics', filter = { buf = 0 }})
        else
          util.echo('No diagnostics in current buffer.', 'ModeMsg')
        end
      end,
      desc = 'Toggle diagnostics',
    },
  },

  opts = {
    auto_close = true,
    win = { size = 5 },
  }
}
