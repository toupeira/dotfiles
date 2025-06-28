local util = require('util')

return {
  'nvim-neo-tree/neo-tree.nvim',
  lazy = false,

  dependencies = {
    'MunifTanjim/nui.nvim',
  },

  cmd = {
    'Neotree',
  },

  keys = {
    { '-', '<Cmd>Neotree toggle reveal<CR>', desc = 'Toggle Neotree sidebar'},
    { '_', '<Cmd>Neotree toggle reveal position=current<CR>', desc = 'Toggle Neotree window'},
  },

  opts = {
    default_component_configs = {
      git_status = {
        symbols = {
          added     = '',
          deleted   = '',
          modified  = '',
          renamed   = '',
          untracked = '',
          ignored   = '◌',
          unstaged  = '○',
          staged    = '●',
          conflict  = '',
        },
      },
    },

    window = {
      width = 30,

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
      bind_to_cwd = false,
      follow_current_file = { enabled = true },
      hijack_netrw_behavior = 'open_current',

      window = {
        mappings = {
          ['[c'] = 'prev_git_modified',
          [']c'] = 'next_git_modified',
          ['[g'] = false,
          [']g'] = false,

          ['ga'] = 'git_add_file',
          ['gu'] = 'git_unstage_file',
        }
      }
    },
  },
}
