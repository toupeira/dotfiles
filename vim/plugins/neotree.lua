local util = require('util')

return {
  'nvim-neo-tree/neo-tree.nvim',
  lazy = false,
  dependencies = {
    'MunifTanjim/nui.nvim',
  },

  cmd = 'Neotree',
  keys = {
    { '-', '<Cmd>Neotree toggle reveal action=show<CR>', desc = 'Toggle Neotree sidebar'},
    { '_', '<Cmd>Neotree toggle reveal position=current<CR>', desc = 'Toggle Neotree window'},
  },

  opts = {
    window = {
      width = 35,

      mappings = {
        ['<Tab>'] = 'toggle_node',
        ['i'] = {
          function(state)
            require('neo-tree/sources/common/commands').show_file_details(state)
            util.nmap('i', ':bwipeout', { buffer = true })
          end,
          desc = 'Show file details',
        },
        ['q'] = {
          function(state)
            local buffers = vim.tbl_filter(
              function(buf) return buf.name ~= '' end,
              vim.fn.getbufinfo({ buflisted = true })
            )

            if #buffers == 0 then
              vim.cmd.quit()
            else
              require('neo-tree/sources/common/commands').close_window(state)
            end
          end,
          desc = 'Close Neotree',
        },
      },
    },

    filesystem = {
      follow_current_file = { enabled = true },
      hijack_netrw_behavior = 'open_current',
    },
  },
}
