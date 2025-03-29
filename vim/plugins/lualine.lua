local util = require('util')

return {
  'nvim-lualine/lualine.nvim',
  event = 'VeryLazy',

  init = function()
    vim.o.statusline = ' '
  end,

  opts = function()
    local green = util.get_color('DiagnosticOk')
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
        'mason',
        'neo-tree',
        'quickfix',
        'trouble',
      },

      sections = {
        lualine_a = {
          { 'mode',
            separator = '',
            fmt = function(str)
              if str == 'V-LINE'  then return 'V' end
              if str == 'V-BLOCK' then return '^V' end
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
          {
            'filetype',
            icon_only = true,
            separator = '',
            padding = { left = 1, right = 0 },
            fmt = function(icon)
              return #icon > 0 and icon or ''
            end,
          },
          { 'filename',
            separator = '',
            padding = { left = 0 },
            shorting_target = 0,
            newfile_status = true,
            symbols = {
              modified = '●',
              readonly = '󰌾',
              newfile = '[New]',
            },
            color = function()
              return {
                fg = vim.bo.modified and orange or 'white',
                gui = 'bold'
              }
            end,
          },
          { 'searchcount',
            icon = '',
            separator = '',
            color = { fg = green, gui = 'bold' },
          },
        },

        lualine_x = {
          { 'diagnostics',
            sources = { 'nvim_diagnostic' },
            symbols = { error = '● ', warn = '● ', info = '● ', hint = '● ' },
            colored = true,
          },
          { function()
              local max = math.max(10, vim.fn.winwidth(0) - 60 - #vim.fn.expand('%:t'))
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
          { 'filetype',
            fmt = function(filetype)
              local bufnr = vim.api.nvim_get_current_buf()
              local lsp_clients = vim.lsp.get_clients({ bufnr = bufnr })
              if next(lsp_clients) == nil then
                return filetype
              end

              local names = {}
              for _, client in pairs(lsp_clients) do
                table.insert(names, client.name)
              end
              return table.concat(names, '|')
            end
          },
        },

        lualine_z = {
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
          { 'selectioncount',
            icon = '󱄽',
            separator = '',
            padding = { left = 0, right = 1 },
          },
        },
      },
    }
  end
}
