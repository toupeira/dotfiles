local util = require('util')

local nmap = util.nmap

-- return {
--   'romgrk/barbar.nvim',
--   opts = {
--     auto_hide = 1,
--     minimum_padding = 2,
--     maximum_padding = 2,
--     icons = {
--       preset = 'slanted',
--       separator = { left = '', right = '' },
--       inactive = { separator = { left = '', right = '' } },
--       button = '',
--     }
--   }
-- }

return {
  'akinsho/bufferline.nvim',

  opts = {
    options = {
      always_show_bufferline = false,
      indicator = { style = 'none' },
      separator_style = 'slope',
      show_buffer_close_icons = false,
      show_close_icon = false,
      sort_by = 'insert_after_current',
      tab_size = 1,

      close_command = 'BufDel %d',
      right_mouse_command = nil,
    },
  },

  config = function(_, opts)
    local bufferline = require('bufferline')
    local orange = util.get_color('DiagnosticWarn')

    opts = util.merge(opts, {
      options = {
        style_preset = bufferline.style_preset.no_italic,
      },

      highlights = {
        modified = { fg = orange },
        modified_visible = { fg = orange },
        modified_selected = { fg = orange },
      }
    })

    -- tweak slope separators
    require('bufferline.constants').sep_chars.slope = { '', '' }

    bufferline.setup(opts)

    nmap('<Leader>n', function() bufferline.cycle(1) end, 'Go to next buffer')
    nmap('<Leader>p', function() bufferline.cycle(-1) end, 'Go to previous buffer')
    nmap('<Leader>`', bufferline.pick, 'Pick buffer')
    nmap('<Leader>~', bufferline.close_with_pick, 'Pick buffer to close')

    for i = 1, 9 do
      nmap('<Leader>' .. i, function() bufferline.go_to(i) end, 'Go to buffer ' .. i)
    end

    -- show bufferline when tabs are used
    -- TODO: fix upstream
    -- vim.api.nvim_create_autocmd('TabEnter', {
    --   pattern = '*',
    --   group = 'BufferlineCmds',
    --   callback = function()
    --     local utils = require('bufferline.utils')
    --     local count = math.max(utils.get_tab_count(), utils.get_buf_count())
    --     local status = count > 1 and 2 or 0
    --     if vim.o.showtabline ~= status then vim.o.showtabline = status end
    --   end
    -- })
  end
}
