local util = require('util')
local nmap = util.nmap
local vmap = util.vmap
local imap = util.imap

return {
  'echasnovski/mini.nvim',

  config = function()
    -- mini.misc -------------------------------------------------------
    require('mini.misc').setup_auto_root(
      { '.git' }, vim.fs.dirname
    )

    -- mini.starter ----------------------------------------------------
    local starter = require('mini.starter')
    local is_home = vim.fn.getcwd() == os.getenv('HOME')

    util.command('MiniStarter', 'lua MiniStarter.open()', 'Open start screen')
    util.autocmd('User', 'VeryLazy', 'lua MiniStarter.refresh()')

    starter.setup({
      evaluate_single = true,
      silent = true,
      query_updaters = 'abcdefghijklmnopqrstuvwxyz0123456789',

      items = {
        { section = 'Builtin actions', name = 'Edit new file', action = 'enew' },
        { section = 'Builtin actions', name = 'Insert mode', action = function () vim.cmd.enew(); vim.cmd.startinsert() end },
        { section = 'Builtin actions', name = 'Quit', action = 'quitall' },

        not is_home and starter.sections.recent_files(9, true, function(path)
          local dir = vim.fn.fnamemodify(path, ':.:h')
          if dir == '.' then
            return ''
          end

          local basename = vim.fn.fnamemodify(path, ':t')
          local max = vim.api.nvim_win_get_width(0) - #basename - 40
          if #dir > max then
            dir = '…' .. dir:sub(-max)
          end

          return string.format(' (%s)', dir)
        end),

        { section = 'Bookmarks', name = 'vimrc', action = 'edit ~/.config/nvim/init.lua' },
        { section = 'Bookmarks', name = 'gitconfig', action = 'edit ~/.config/git/config' },
        { section = 'Bookmarks', name = 'tmux.conf', action = 'edit ~/.config/tmux/tmux.conf' },
      },

      footer = function()
        local stats = require('lazy').stats()
        return string.format(
          'Started in %dms, loaded %d/%d plugins.',
          stats.times.LazyDone,
          stats.loaded,
          stats.count
        )
      end,


      content_hooks = {
        starter.gen_hook.adding_bullet(),
        starter.gen_hook.indexing('all', { 'Builtin actions', 'Bookmarks' }),
        starter.gen_hook.aligning('center', 'top'),
        starter.gen_hook.padding(0, vim.o.lines / (is_home and 3 or 5)),
      },
    })

    util.autocmd_once('User', 'VeryLazy', function()
      -- mini.ai -------------------------------------------------------
      require('mini.ai').setup()

      -- mini.align ----------------------------------------------------
      require('mini.align').setup({
        mappings = {
          start = '<Leader>a',
          start_with_preview = '<Leader>A',
        },
      })

      -- mini.basics ---------------------------------------------------
      require('mini.basics').setup({
        options      = { basic = false, win_borders = 'bold' },
        mappings     = { basic = false, move_with_alt = true },
        autocommands = { basic = true },
      })

      -- mini.bracketed ------------------------------------------------
      require('mini.bracketed').setup({
        diagnostic = { suffix = 'e' },

        comment    = { suffix = '' }, -- ']c' used by treesitter-textobjects
        file       = { suffix = '' }, -- ']f' not useful
        oldfile    = { suffix = '' }, -- ']o' not useful
        treesitter = { suffix = '' }, -- ']t' doesn't work well
      })

      -- mini.clue -----------------------------------------------------
      local clue = require('mini.clue')
      vim.o.timeoutlen = 500
      clue.setup({
        triggers = {
          -- Leader triggers
          { mode = 'n', keys = '<Leader>' },
          { mode = 'v', keys = '<Leader>' },
          { mode = 'n', keys = '<LocalLeader>' },
          { mode = 'v', keys = '<LocalLeader>' },
          { mode = 'n', keys = '<F1>' },

          -- Built-in completion
          { mode = 'i', keys = '<C-x>' },

          -- `g` key
          { mode = 'n', keys = 'g' },
          { mode = 'v', keys = 'g' },

          -- Marks
          { mode = 'n', keys = "'" },
          { mode = 'n', keys = '`' },
          { mode = 'v', keys = "'" },
          { mode = 'v', keys = '`' },

          -- Registers
          { mode = 'n', keys = '"' },
          { mode = 'v', keys = '"' },
          { mode = 'i', keys = '<C-r>' },
          { mode = 'c', keys = '<C-r>' },

          -- Window commands
          { mode = 'n', keys = '<C-w>' },

          -- `z` key
          { mode = 'n', keys = 'z' },
          { mode = 'v', keys = 'z' },

          -- mini.bracketed and others
          { mode = 'n', keys = ']' },
          { mode = 'n', keys = '[' },

          -- mini.surround
          { mode = 'n', keys = 'S' },
          { mode = 'v', keys = 'S' },
        },

        clues = {
          clue.gen_clues.builtin_completion(),
          clue.gen_clues.g(),
          clue.gen_clues.marks(),
          clue.gen_clues.registers(),
          clue.gen_clues.windows(),
          clue.gen_clues.z(),

          { mode = 'n', keys = '<Leader><F1>', desc = '➜ help' },
          { mode = 'n', keys = '<Leader><Leader>', desc = '➜ resume fuzzy search' },
          { mode = 'n', keys = '<Leader><Leader>d', desc = '➜ lsp/treesitter' },
          { mode = 'n', keys = '<Leader><Leader>g', desc = '➜ git' },
          { mode = 'n', keys = '<Leader>d', desc = '➜ lsp/treesitter' },
          { mode = 'n', keys = '<Leader>dR', desc = 'Smart rename' },
          { mode = 'n', keys = '<Leader>g', desc = '➜ git' },
          { mode = 'v', keys = '<Leader>g', desc = '➜ git' },
        },

        window = {
          config = { width = 40, border = 'rounded' },
          delay = vim.o.timeoutlen,
        },
      })

      -- mini.move -----------------------------------------------------
      require('mini.move').setup()
      imap('<M-H>', ':lua MiniMove.move_line("left")',  'Move line left')
      imap('<M-J>', ':lua MiniMove.move_line("down")',  'Move line down')
      imap('<M-K>', ':lua MiniMove.move_line("up")',    'Move line up')
      imap('<M-L>', ':lua MiniMove.move_line("right")', 'Move line right')

      -- mini.operators ------------------------------------------------
      require('mini.operators').setup({
        sort = { prefix = '' },
        exchange = { prefix = 'ge' },
      })
      vmap('D', 'gm', { remap = true }, 'Duplicate selection')

      -- mini.pairs ----------------------------------------------------
      local pairs = require('mini.pairs')
      pairs.setup({
        modes = {
          command = false,
          terminal = false,
        },

        mappings = {
          ['('] = { neigh_pattern = '[^\\][ \n]' },
          ['['] = { neigh_pattern = '[^\\][ \n]' },
          ['{'] = { neigh_pattern = '[^\\][ \n]' },
          ['"'] = { neigh_pattern = '[^%a)}%]\\][ \n]' },
          ["'"] = { neigh_pattern = '[^%a)}%]\\][ \n]' },
          ['`'] = { neigh_pattern = '[^%a)}%]\\][ \n]' },

          [' '] = {
            action = 'closeopen',
            pair = '  ',
            neigh_pattern = '[%(%[{][%)%]}]',
          },
        }
      })

      -- re-add undo chain to <CR> from core/keymaps.lua
      local cr = pairs.cr
      pairs.cr = function(...)
        return "u" .. cr(...)
      end

      -- mini.surround -------------------------------------------------
      require('mini.surround').setup({
        mappings = {
          add = 'Sa',
          delete = 'Sd',
          find = 'Sf',
          find_left = 'SF',
          highlight = 'Sh',
          replace = 'Sr',
          update_n_lines = 'Sn',
        },
      })

      -- mini.trailspace -----------------------------------------------
      require('mini.trailspace').setup()
      nmap('<Leader>$', function()
        if vim.bo.modifiable then
          MiniTrailspace.trim()
        end
      end, 'Trim trailing whitespace')
      util.hl_set('MiniTrailspace', { bg = util.get_color('Pmenu', 'bg') })
    end)
  end
}
