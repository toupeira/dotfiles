local util = require('util')
local nmap = util.nmap
local vmap = util.vmap
local imap = util.imap

return {
  'echasnovski/mini.nvim',

  config = function()
    -- mini.clue -----------------------------------------------------
    local clue = require('mini.clue')

    util.autocmd('User', 'MiniStarterOpened', function(event)
      MiniClue.enable_buf_triggers(event.buf)
      util.nmap('g', function()
        MiniStarter.add_to_query('g', event.buf)
      end, { buffer = event.buf, nowait = true, force = true })
    end)

    vim.o.timeoutlen = 500

    clue.setup({
      triggers = {
        -- Leader triggers
        { mode = 'n', keys = '<Leader>' },
        { mode = 'v', keys = '<Leader>' },
        { mode = 'n', keys = '<Leader>!' },
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

        -- mini.comment
        { mode = 'n', keys = 'gc' },

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
        { mode = 'n', keys = '<Leader>a', desc = '➜ ai' },
        { mode = 'n', keys = '<Leader>L', desc = '➜ lsp' },
        { mode = 'n', keys = '<Leader>S', desc = '➜ sessions' },
        { mode = 'n', keys = '<Leader>g', desc = '➜ git' },
        { mode = 'v', keys = '<Leader>g', desc = '➜ git' },
        { mode = 'n', keys = '<Leader><Leader>L', desc = '➜ lsp' },
        { mode = 'n', keys = '<Leader><Leader>g', desc = '➜ git' },
      },

      window = {
        config = { width = 40, border = 'rounded' },
        delay = vim.o.timeoutlen,
      },
    })

    -- mini.icons ------------------------------------------------------
    require('mini.icons').setup({
      lsp = {
        ['function'] = { glyph = '󰊕' },
      }
    })
    MiniIcons.mock_nvim_web_devicons()

    -- mini.misc -------------------------------------------------------
    require('mini.misc').setup_auto_root(
      { '.git', '.obsidian' }, vim.fs.dirname
    )

    -- mini.sessions -------------------------------------------------
    require('mini.sessions').setup({
      autoread = true,
      file = '.session.vim',
      directory = '',
      verbose = { read = false, write = false, delete = false },

      hooks = {
        post = {
          read   = function() util.notify('Session:', { annote = 'Restored.', level = 'WARN' }) end,
          write  = function() util.notify('Session:', { annote = 'Saved.' }) end,
          delete = function() util.notify('Session:', { annote = 'Deleted.', level = 'WARN' }) end,
        }
      }
    })

    if not MiniSessions.get_latest() then
      vim.g.minisessions_disable = true
    end

    nmap('<Leader>SS', function()
      vim.g.minisessions_disable = false

      local name = util.project_path(0)
      MiniSessions.write('.session.vim', { force = true })
    end, 'Save current session')

    nmap('<Leader>SR', function()
      vim.g.minisessions_disable = false
      local original_write = MiniSessions.write
      MiniSessions.write = function() end
      MiniSessions.read()
      MiniSessions.write = original_write
    end, 'Restore saved session')

    nmap('<Leader>SD', function()
      vim.g.minisessions_disable = false
      local ok, error = pcall(MiniSessions.delete, nil, { force = true })
      if not ok then
        util.error(error)
      end
    end, 'Delete saved session')

    -- mini.starter ----------------------------------------------------
    local starter = require('mini.starter')
    local fzf = require('fzf-lua')

    util.command('MiniStarter', 'lua MiniStarter.open()', 'Open start screen')
    util.autocmd('User', 'VeryLazy', 'lua MiniStarter.refresh()')

    starter.setup({
      evaluate_single = true,
      silent = true,
      query_updaters = 'abcdefghijklmnopqrstuvwxyz0123456789',

      items = {
        -- default actions
        { section = 'Actions', name = 'Edit new file', action = 'enew' },
        { section = 'Actions', name = 'Insert mode', action = 'enew | startinsert' },

        function()
          if util.tab_count() > 1 then
            return { section = 'Actions', name = 'Close tab', action = 'tabclose' }
          else
            return { section = 'Actions', name = 'Quit', action = 'quitall' }
          end
        end,

        -- recent files
        starter.sections.recent_files(9, not util.is_home, function(path)
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

        -- FZF shortcuts
        vim.tbl_map(function(item)
          return item and {
            section = 'Search',
            name = item.name,
            action = function()
              vim.fn.feedkeys(vim.g.mapleader .. item.key)
            end,
          }
        end, {
          { name = 'Files',   key = 'f' },
          { name = 'Grep',    key = 'r' },
          { name = 'History', key = 'h' },
          not util.is_home and { name = 'Tags', key = 'T' },
        })
      },

      footer = function()
        local tab_count = util.tab_count()
        if tab_count > 1 then
          return tab_count .. ' tabs open.'
        end

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
        starter.gen_hook.indexing('all', { 'Actions', 'Search' }),
        starter.gen_hook.aligning('center', 'top'),
        starter.gen_hook.padding(0, vim.o.lines / 5.5),
      },
    })

    util.autocmd('User', 'VeryLazy', { once = true }, function()
      -- mini.ai -------------------------------------------------------
      require('mini.ai').setup()

      -- mini.align ----------------------------------------------------
      require('mini.align').setup({
        mappings = {
          start = '<Leader>=',
          start_with_preview = '<Leader>+',
        },
      })

      -- mini.basics ---------------------------------------------------
      require('mini.basics').setup({
        options      = { basic = false, win_borders = 'bold' },
        mappings     = { basic = false, move_with_alt = true },
        autocommands = { basic = true },
      })
      util.unmap('n', '<LocalLeader>b')
      util.unmap('n', '<LocalLeader>h')
      util.unmap('n', '<LocalLeader>i')

      -- mini.bracketed ------------------------------------------------
      require('mini.bracketed').setup({
        comment    = { suffix = '' }, -- ']c' used by vim/gitsigns
        file       = { suffix = '' }, -- ']f' not useful
        oldfile    = { suffix = '' }, -- ']o' not useful
      })

      local original_diagnostic = MiniBracketed.diagnostic
      MiniBracketed.diagnostic = function(...)
        if vim.diagnostic.show_current_line_id then
          vim.diagnostic.hide_current_line()
        end

        original_diagnostic(...)
        vim.diagnostic.show_current_line()
      end

      for _, key in ipairs({ 'b', 'e', 'i', 'j', 'l', 'q', 't', 'u', 'w', 'x', 'y' }) do
        for _, mode in ipairs({ 'n', 'x', 'o' }) do
          local next = vim.fn.maparg(']' .. key, mode, false, true)
          local prev = vim.fn.maparg('[' .. key, mode, false, true)

          if next.rhs and prev.rhs then
            local next_rhs = next.rhs:gsub('<Cmd>', ''):gsub('<CR>', '')
            local prev_rhs = prev.rhs:gsub('<Cmd>', ''):gsub('<CR>', '')

            local next_repeat, prev_repeat = util.make_repeatable(
              function() pcall(vim.cmd, next_rhs) end,
              function() pcall(vim.cmd, prev_rhs) end
            )

            util.map(mode, ']' .. key, next_repeat, { force = true }, next.desc)
            util.map(mode, '[' .. key, prev_repeat, { force = true }, prev.desc)
          end
        end
      end

      -- mini.comment --------------------------------------------------
      require('mini.comment').setup()

      -- mini.diff -----------------------------------------------------
      local diff = require('mini.diff')

      -- disable all mappings and default diffing
      for key, _ in pairs(diff.config.mappings) do
        diff.config.mappings[key] = ''
      end

      diff.setup({
        source = diff.gen_source.none(),
        options = { wrap_goto = true },
      })

      -- mini.jump -----------------------------------------------------
      require('mini.jump').setup()

      local original_jump = MiniJump.jump
      local repeat_move = require('nvim-treesitter.textobjects.repeatable_move')

      MiniJump.jump = function(...)
        repeat_move.last_move = nil
        return original_jump(...)
      end

      nmap(';', function ()
        if repeat_move.last_move then
          repeat_move.repeat_last_move()
        else
          MiniJump.jump()
        end
      end, { force = true }, 'Repeat jump')

      nmap('|', function ()
        if repeat_move.last_move then
          repeat_move.repeat_last_move_opposite()
        else
          MiniJump.jump(nil, true)
        end
      end, { force = true }, 'Repeat jump backward')

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
      require('mini.pairs').setup({
        modes = {
          command = false,
          terminal = false,
        },

        mappings = {
          ['('] = { neigh_pattern = '[^\\][%s)}%]]' },
          ['['] = { neigh_pattern = '[^\\][%s)}%]]' },
          ['{'] = { neigh_pattern = '[^\\][%s)}%]]' },
          ['"'] = { neigh_pattern = '[^\\%a"][%s)}%]]' },
          ["'"] = { neigh_pattern = "[^\\%a'][%s)}%]]" },
          ['`'] = { neigh_pattern = '[^\\%a`][%s)}%]]' },

          [' '] = {
            action = 'closeopen',
            pair = '  ',
            neigh_pattern = '[({%[][)}%]]',
          },
        }
      })

      -- re-add undo chain to <CR> from core/keymaps.lua
      local original_cr = MiniPairs.cr
      MiniPairs.cr = function(...)
        return "u" .. original_cr(...)
      end

      -- mini.pick -----------------------------------------------------
      require('mini.pick').setup({
        options = { content_from_bottom = true },
        window = { prompt_prefix = '» ' },
      })

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
      nmap('<Leader>W', function()
        if vim.bo.modifiable then
          MiniTrailspace.trim()
        end
      end, 'Trim trailing whitespace')
    end)
  end,
}
