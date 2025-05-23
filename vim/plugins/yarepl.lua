local util = require('util')
local map = util.map
local nmap = util.nmap

return {
  'milanglacier/yarepl.nvim',
  cmd = {
    'REPLClose',
    'REPLExec',
    'REPLFocus',
    'REPLHide',
    'REPLHideOrFocus',
    'REPLSendLine',
    'REPLSendOperator',
    'REPLSendVisual',
    'REPLSourceOperator',
    'REPLSourceVisual',
    'REPLStart',
    'REPLSwap',
  },

  opts = function()
    local wincmd = function(bufnr, _name)
      local split, width, height
      width = 1
      height = 1
      if vim.o.columns > 100 and vim.o.columns >= vim.o.lines * 3 then
        split = 'right'
        width = 0.4
      else
        split = 'below'
        height = 0.3
      end

      vim.api.nvim_open_win(bufnr, true, {
        split = split,
        width = math.floor(vim.o.columns * width),
        height = math.floor(vim.o.lines * height),
      })
    end

    return {
      buflisted = false,
      wincmd = wincmd,
      metas = {
        aider = util.merge(require('yarepl.extensions.aider').create_aider_meta(), {
          wincmd = wincmd,
        }),
        ruby = { cmd = 'pry', formatter = 'bracketed_pasting' },

        aichat = false,
        radian = false,
        python = false,
        R = false,
        zsh = false,
      },
    }
  end,

  init = function()
    local function map_toggle(modes, lhs, name, desc)
      map(modes, lhs, function()
        if vim.bo.filetype == 'fzf' then return lhs end

        vim.schedule(function()
          local repl = require('yarepl')._get_repl(nil, name, vim.api.nvim_get_current_buf())
          local path = vim.fn.expand('%:p')

          if repl then
            vim.cmd.REPLHideOrFocus(name)
            if vim.bo.filetype == 'REPL' then
              vim.cmd.startinsert()
            end
          else
            vim.cmd.REPLStart(name)
          end

          if name == 'aider' and vim.fn.filereadable(path) == 1 then
            vim.cmd.AiderExec('/add ' .. path)
          end
        end)
      end, { expr = true, desc = desc })
    end

    map_toggle({ 'n', 't' }, '<M-z>', 'bash', 'Toggle terminal')
    nmap('<Leader>!', '<M-z>', 'Toggle terminal', { remap = true })

    util.autocmd({ 'BufWinEnter', 'WinEnter' }, '#*#*', 'startinsert!')
  end,
}
