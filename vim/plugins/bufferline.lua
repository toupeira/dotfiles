local util = require('util')

local nmap = util.nmap

return {
  'akinsho/bufferline.nvim',
  event = 'VeryLazy',

  opts = {
    options = {
      always_show_bufferline = false,
      indicator = { style = 'none' },
      separator_style = 'slope',
      show_buffer_close_icons = false,
      show_close_icon = false,
      sort_by = 'insert_after_current',
      tab_size = 1,

      close_command = util.close_buffer,
      right_mouse_command = nil,

      -- luacheck: globals MiniIcons
      get_element_icon = function(element)
        local icon
        if element.filetype ~= '' then
          icon = MiniIcons.get('filetype', element.filetype)
        else
          icon = MiniIcons.get('extension', element.extension)
        end

        return icon
      end,
    },

    highlights = {
      buffer_selected = { fg = 'white' },
      tab_selected = { fg = 'white', bold = true },

      buffer_visible = {
        fg = { highlight = 'Normal', attribute = 'fg' },
        bg = { highlight = 'Normal', attribute = 'bg' },
      },

      duplicate_selected = {
        fg = { highlight = 'DiagnosticInfo', attribute = 'fg' },
        bold = true,
      },

      duplicate_visible = {
        fg = { highlight = 'DiagnosticInfo', attribute = 'fg' },
      },
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
    local config = require('bufferline.config')
    util.autocmd({ 'TabNew', 'TabClosed' }, function()
      config.options.always_show_bufferline = #vim.fn.gettabinfo() > 1
    end)
  end,
}
