return {
  'b0o/incline.nvim',
  event = 'VeryLazy',

  opts = {
    window = {
      margin = { horizontal = 0, vertical = 0 },
    },
  },

  config = function(_, opts)
    local separator = '  '
    local comment = require('util').get_color('Comment')

    local function get_window_count()
      local windows = vim.api.nvim_tabpage_list_wins(0)
      return #vim.tbl_filter(function(win)
        return vim.api.nvim_win_get_config(win).relative == ''
      end, windows)
    end

    local function get_filename(props)
      if props.windows <= 1 then return '' end

      local path = vim.api.nvim_buf_get_name(props.buf)
      local filename = vim.fn.fnamemodify(path, ':t')
      if filename == '' then filename = '[No Name]' end

      return {
        ' ' .. filename,
        gui = 'bold',
        guifg = props.focused and 'white' or comment,
      }
    end

    local aerial
    local function get_symbols(props)
      if not props.focused then return '' end

      if not aerial then
        aerial = require('lualine/components/aerial')({
          self = { section = 'x' },
          icons_enabled = true,
          sep = separator,
        })
      end

      local symbols = vim.api.nvim_eval_statusline(
        aerial:get_status(), { winid = props.win }
      ).str

      if symbols ~= '' then
        return (props.windows > 1 and separator or '') .. symbols
      end
    end

    opts.render = function(props)
      props.windows = get_window_count()

      return {
        get_filename(props),
        get_symbols(props),
      }
    end

    require('incline').setup(opts)
  end
}
