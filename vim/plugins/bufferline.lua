local util = require('util')

local nmap = util.nmap

return {
  'akinsho/bufferline.nvim',

  config = function()
    local bufferline = require('bufferline')
    local orange = util.get_color('DiagnosticWarn')

    nmap('<Leader>n', function() bufferline.cycle(1) end, 'Go to next buffer')
    nmap('<Leader>p', function() bufferline.cycle(-1) end, 'Go to previous buffer')
    nmap('<Leader>`', bufferline.pick, 'Pick buffer')
    nmap('<Leader>~', bufferline.close_with_pick, 'Pick buffer to close')

    for i = 1, 9 do
      nmap('<Leader>' .. i, function() bufferline.go_to(i) end, 'Go to buffer ' .. i)
    end

    -- tweak slope separators
    require('bufferline.constants').sep_chars.slope = { '', '' }

    bufferline.setup({
      options = {
        always_show_bufferline = false,
        indicator = { style = 'none' },
        separator_style = 'slope',
        show_buffer_close_icons = false,
        show_close_icon = false,
        sort_by = 'insert_after_current',
        style_preset = bufferline.style_preset.no_italic,
        tab_size = 1,

        close_command = 'BufDel %d',
        right_mouse_command = nil,

        diagnostics = 'nvim_lsp',
        diagnostics_indicator = function(count, _level)
          return '● ' .. count
        end,
      },

      highlights = {
        buffer_visible = { bold = true },
        error_visible = { bold = true },
        warning_visible = { bold = true },
        info_visible = { bold = true },
        hint_visible = { bold = true },

        modified = { fg = orange },
        modified_visible = { fg = orange },
        modified_selected = { fg = orange },
      },
    })

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
