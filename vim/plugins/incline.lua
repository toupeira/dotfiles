local util = require('util')

return {
  'b0o/incline.nvim',
  event = 'VeryLazy',

  opts = {
    window = {
      margin = { horizontal = 0, vertical = 0 },
    },
  },

  config = function(_, opts)
    local helpers = require('incline.helpers')
    local icons = require('nvim-web-devicons')

    local comment = util.get_color('Comment')
    local separator = {  '  ', group = 'Comment' }

    local function get_filename(props)
      if props.windows <= 1 or props.buffers <= 1 then
        return
      end

      local path = vim.api.nvim_buf_get_name(props.buf)
      local filename = vim.fn.fnamemodify(path, ':t')
      if filename == '' then
        filename = '[No Name]'
      end

      local icon = icons.get_icon(
        filename, vim.fn.fnamemodify(filename, ':e'),
        { default = true }
      )

      return {
        icon .. ' ' .. filename,
        gui = 'bold',
        guifg = props.focused and 'white' or comment,
      }
    end

    local aerial
    local function get_symbols(props)
      if not props.focused then
        return
      end

      if not aerial then
        local ok
        ok, aerial = pcall(require, 'lualine/components/aerial')
        if not ok then aerial = nil; return end

        aerial = aerial({
          self = { section = 'c' },
          icons_enabled = true,
          sep = separator[1],
          sep_highlight = separator.group,
        })
      end

      local aerial_statusline = vim.api.nvim_win_call(props.win, function()
        return aerial:get_status { winid = props.win }
      end)

      if aerial_statusline == '' then
        return
      end

      return {
        helpers.eval_statusline(aerial_statusline, {
          winid = props.win,
          highlights = props.focused,
        }),
        group = 'Comment',
      }
    end

    opts.render = function(props)
      props.windows = util.window_count()
      props.buffers = util.buffer_count()

      local sections = {
        get_symbols(props),
        get_filename(props),
      }

      local result = {}
      for _, section in pairs(sections) do
        if section and section ~= '' and not (type(section) == 'table' and #section == 0) then
          if #result > 0 then
            table.insert(result, separator)
          end
          table.insert(result, section)
        end
      end

      return result
    end

    require('incline').setup(opts)

    -- reduce updates to avoid cursor lag
    vim.api.nvim_clear_autocmds({
      event = { 'CursorMoved', 'CursorMovedI' },
      group = 'incline',
    })
  end
}
