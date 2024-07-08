return {
  'nvim-neo-tree/neo-tree.nvim',
  lazy = false,
  dependencies = {
    'MunifTanjim/nui.nvim',
  },

  keys = {
    { '-', '<Cmd>Neotree toggle reveal<CR>', desc = 'Toggle Neotree sidebar'},
    { '_', '<Cmd>Neotree toggle reveal current<CR>', desc = 'Toggle Neotree window'},
  },

  opts = {
    window = {
      width = 30,
      mappings = {
        ['<Tab>'] = 'toggle_node',
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
  }
}
