local util = require('util')

return {
  'nvim-lualine/lualine.nvim',

  opts = function()
    local orange = util.get_color('DiagnosticWarn')
    local magenta = util.get_color('Statement')

    vim.opt.shortmess:append('S') -- searchcount
    vim.o.showcmd = false         -- selectioncount

    return {
      options = {
        globalstatus = true,
        section_separators   = vim.o.termguicolors and { left = '', right = '' } or { left = '', right = '' },
        component_separators = vim.o.termguicolors and { left = '', right = '' } or { left = '\\', right = '/' },
        icons_enabled = vim.o.termguicolors,
        colored = false,
      },

      extensions = {
        'aerial',
        'fugitive',
        'fzf-custom',
        'lazy',
        'man',
        'quickfix',
      },

      sections = {
        lualine_a = {
          { 'mode',
            fmt = function(str)
              return str:sub(1,1):lower()
            end
          },
        },

        lualine_b = {
          { 'branch',
            fmt = function(branch)
              if vim.o.termguicolors and (branch == 'main' or branch == 'master') then
                return ''
              end

              branch = vim.fn.pathshorten(branch)
              if #branch > 20 then
                return branch:sub(0, 20) .. '…'
              else
                return branch
              end
            end,
          },
        },

        lualine_c = {
          { 'filename',
            icon = '',
            shorting_target = 0,
            newfile_status = true,
            symbols = {
              modified = '●',
              readonly = '󰌾',
              newfile = '',
            },
            color = function()
              return {
                fg = vim.bo.modified and orange or 'white',
                gui = 'bold'
              }
            end,
          },
          { 'diagnostics',
            sources = { 'nvim_diagnostic' },
            symbols = { error = '● ', warn = '● ', info = '● ', hint = '● ' },
            colored = true,
          },
        },

        lualine_x = {
          { function()
              local max = math.max(10, vim.fn.winwidth(0) - 50 - #vim.fn.expand('%:t'))
              return util.project_path(max)
            end,
            icon = '',
          },
          { function()
              return vim.o.fileencoding == 'utf-8' and '' or vim.o.fileencoding
            end,
            color = { fg = magenta },
          },
          { 'fileformat',
            symbols = { unix = '' },
            color = { fg = magenta },
          },
        },

        lualine_y = {
          'filetype',
        },

        lualine_z = {
          { 'searchcount',
            icon = '',
            separator = '',
            padding = { left = 1, right = 0 },
          },
          { 'selectioncount',
            icon = '󱄽',
            separator = '',
            padding = { left = 1, right = 0 },
            fmt = function(string)
              if string ~= '' then return '[' .. string .. ']' end
            end
          },
          { function()
              local line = vim.fn.line('.')
              return (line < 10 and ' ' or '') .. line
            end,
            separator = '',
            padding = { left = 1, right = 0 },
          },
          { '/%L',
            type = 'stl',
            separator = '',
            padding = 0,
            color = { gui = 'NONE' },
          },
          { function()
              local col = vim.fn.virtcol('.')
              return ':' .. col .. (col < 10 and ' ' or '')
            end,
            separator = '',
            padding = { left = 0, right = 1 },
          },
        },
      },
    }
  end
}
