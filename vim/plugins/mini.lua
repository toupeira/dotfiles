-- vim: foldmethod=marker foldlevel=0
-- luacheck: globals MiniBracketed MiniClue MiniIcons MiniJump MiniPairs MiniSessions MiniStarter MiniTrailspace

local util = require('util')
local nmap = util.nmap
local vmap = util.vmap
local imap = util.imap

return {
  'nvim-mini/mini.nvim',
  event = 'VeryLazy',

  init = function()
    -- mini.clue {{{
    util.autocmd('User', 'MiniStarterOpened', function(event)
      MiniClue.enable_buf_triggers(event.buf)
      util.nmap('g', function()
        MiniStarter.add_to_query('g', event.buf)
      end, { buffer = event.buf, nowait = true, force = true })
    end)
    -- }}}
    -- mini.icons {{{
    require('mini.icons').setup({
      filetype = {
        help = { glyph = '󰋗' },
        lua = { glyph = '' },
        sh = { glyph = '' },
      },

      lsp = {
        ['function'] = { glyph = '󰊕' },
        method = { glyph = '󰊕' },

        class = { glyph = '' },
        module = { glyph = '' },
        interface = { glyph = '' },

        field = { glyph = '' },
        property = { glyph = '' },
      }
    })
    MiniIcons.mock_nvim_web_devicons()
    -- }}}
    -- mini.misc {{{
    require('mini.misc').setup_auto_root(
      {
        '.git',
        '.obsidian',
        'LICENSE',
        'LICENSE.md',
        'LICENSE.txt',
      }, vim.fs.dirname
    )
    -- }}}
    -- mini.sessions {{{
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
    -- }}}
    -- mini.starter {{{
    local starter = require('mini.starter')

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
    -- }}}
  end,

  config = function()
    -- mini.ai {{{
    require('mini.ai').setup()
    -- }}}
    -- mini.align {{{
    require('mini.align').setup({
      mappings = {
        start = '<Leader>=',
        start_with_preview = '<Leader>+',
      },
    })
    -- }}}
    -- mini.basics {{{
    require('mini.basics').setup({
      options      = { basic = false, win_borders = 'bold' },
      mappings     = { basic = false, move_with_alt = true },
      autocommands = { basic = true },
    })

    util.unmap('n', '<LocalLeader>b')
    util.unmap('n', '<LocalLeader>c')
    util.unmap('n', '<LocalLeader>h')

    util.nmap('<LocalLeader>cc', ':setlocal cursorcolumn! cursorcolumn?<CR>', "Toggle 'cursorcolumn'")
    util.nmap('<LocalLeader>cl', ':setlocal cursorline! cursorline?<CR>', "Toggle 'cursorline'")
    -- }}}
    -- mini.bracketed {{{
    require('mini.bracketed').setup({
      indent  = { options = { change_type = 'diff' }},

      comment = { suffix = '' }, -- ']c' used by vim/gitsigns
      file    = { suffix = '' }, -- ']f' not useful
      oldfile = { suffix = '' }, -- ']o' not useful
    })

    local original_diagnostic = MiniBracketed.diagnostic
    MiniBracketed.diagnostic = function(...)
      if vim.diagnostic.show_current_line_id then
        vim.diagnostic.hide_current_line()
      end

      original_diagnostic(...)
      vim.diagnostic.show_current_line()
    end

    for _, config in pairs(MiniBracketed.config) do
      if config.suffix ~= '' then
        for _, mode in ipairs({ 'n', 'x', 'o' }) do
          local next = vim.fn.maparg(']' .. config.suffix, mode, false, true)
          local prev = vim.fn.maparg('[' .. config.suffix, mode, false, true)

          if next.rhs and prev.rhs then
            local next_rhs = next.rhs:gsub('<Cmd>', ''):gsub('<CR>', '')
            local prev_rhs = prev.rhs:gsub('<Cmd>', ''):gsub('<CR>', '')

            local next_repeat, prev_repeat = util.make_repeatable(
              function() pcall(vim.cmd, next_rhs) end,
              function() pcall(vim.cmd, prev_rhs) end
            )

            util.map(mode, ']' .. config.suffix, next_repeat, { force = true }, next.desc)
            util.map(mode, '[' .. config.suffix, prev_repeat, { force = true }, prev.desc)
          end
        end
      end
    end
    -- }}}
    -- mini.clue {{{
    local clue = require('mini.clue')
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
        { mode = 'n', keys = 'gc' },

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
        { mode = 'n', keys = 'ys' },
      },

      clues = {
        clue.gen_clues.builtin_completion(),
        clue.gen_clues.g(),
        clue.gen_clues.marks(),
        clue.gen_clues.registers(),
        clue.gen_clues.windows(),
        clue.gen_clues.z(),

        { mode = 'n', keys = '<Leader><F1>', desc = '➜ help' },

        { mode = 'n', keys = '<Leader>a', desc = '➜ ai' },
        { mode = 'n', keys = '<Leader>L', desc = '➜ lsp' },
        { mode = 'n', keys = '<Leader>P', desc = '➜ dap' },
        { mode = 'n', keys = '<Leader>S', desc = '➜ sessions' },
        { mode = 'n', keys = '<Leader>g', desc = '➜ git' },
        { mode = 'v', keys = '<Leader>g', desc = '➜ git' },

        { mode = 'n', keys = '<Leader><Leader>', desc = '➜ resume fuzzy search' },
        { mode = 'v', keys = '<Leader><Leader>', desc = '➜ resume fuzzy search' },
        { mode = 'n', keys = '<Leader><Leader>L', desc = '➜ lsp' },
        { mode = 'n', keys = '<Leader><Leader>g', desc = '➜ git' },

        { mode = 'n', keys = '<LocalLeader>c', desc = '➜ cursor' },
        { mode = 'n', keys = '<LocalLeader>g', desc = '➜ git' },
      },

      window = {
        config = { width = 40, border = 'rounded' },
        delay = vim.o.timeoutlen,
      },
    })
    -- }}}
    -- mini.diff {{{
    local diff = require('mini.diff')

    -- disable all mappings and default diffing
    for key, _ in pairs(diff.config.mappings) do
      diff.config.mappings[key] = ''
    end

    diff.setup({
      source = diff.gen_source.none(),
      options = { wrap_goto = true },
    })
    -- }}}
    -- mini.hipatterns {{{
    require('mini.hipatterns').setup()
    -- }}}
    -- mini.jump {{{
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
    -- }}}
    -- mini.keymap {{{
    local keymap = require('mini.keymap')

    keymap.map_multistep('i', '<Tab>', {
      'blink_accept',
      'vimsnippet_next',
      'increase_indent',
      -- jump after next delimiter on current line
      util.merge(
        keymap.gen_step.search_pattern([=[[()\[\]{}"'`]]=], 'cW', {
          side = 'after',
          stopline = function() return vim.fn.line('.') end,
        }), {
          condition = function()
            local col = vim.fn.charcol('.')
            local neigh = vim.fn.getline('.'):sub(col - 1, col)
            return
              -- insert tabs at end of the line
              col ~= vim.fn.charcol('$')
              -- insert tabs unless a delimiter is next to the cursor
              and neigh:match([=[[()%[%]{}"'`]]=])
          end
        }
      ),
    })

    keymap.map_multistep('i', '<S-Tab>', {
      'vimsnippet_prev',
      'decrease_indent',
      -- jump before previous delimiter on current line
      keymap.gen_step.search_pattern([=[[()\[\]{}"'`]]=], 'bW', {
        side = 'before',
        stopline = function() return vim.fn.line('.') end,
      }),
    })
    -- }}}
    -- mini.move {{{
    require('mini.move').setup()
    imap('<M-H>', ':lua MiniMove.move_line("left")',  'Move line left')
    imap('<M-J>', ':lua MiniMove.move_line("down")',  'Move line down')
    imap('<M-K>', ':lua MiniMove.move_line("up")',    'Move line up')
    imap('<M-L>', ':lua MiniMove.move_line("right")', 'Move line right')
    -- }}}
    -- mini.operators {{{
    require('mini.operators').setup({
      sort = { prefix = '' },
      exchange = { prefix = 'ge' },
    })
    vmap('D', 'gm', { remap = true }, 'Duplicate selection')
    -- }}}
    -- mini.pairs {{{
    require('mini.pairs').setup({
      modes = {
        command = false,
        terminal = false,
      },

      mappings = {
        ['('] = { neigh_pattern = '[^\\][%s}%]]' },
        ['['] = { neigh_pattern = '[^\\][%s)}]' },
        ['{'] = { neigh_pattern = '[^\\][%s)%]]' },
        ['"'] = { neigh_pattern = '[^\\%a"][%s)}%]]' },
        ["'"] = { neigh_pattern = "[^\\%a'][%s)}%]]" },
        ['`'] = { neigh_pattern = '[^\\%a`][%s)}%]]' },

        [' '] = {
          action = 'closeopen',
          pair = '  ',
          neigh_pattern = '[({][)}]',
        },
      }
    })
    -- }}}
    -- mini.pick {{{
    require('mini.pick').setup({
      options = { content_from_bottom = true },
      window = { prompt_prefix = '» ' },
    })
    -- }}}
    -- mini.surround {{{
    require('mini.surround').setup({
      respect_selection_type = true,

      mappings = {
        add = 'ys',
        delete = 'ds',
        replace = 'cs',

        find = '',
        find_left = '',
        highlight = '',
        update_n_lines = '',
      },
    })

    util.unmap('x', 'ys')
    vmap('S', ':lua MiniSurround.add("visual")', { silent = true })

    nmap('yss', '^ysg_', { remap = true } , 'Surround current line')

    nmap("ysq", "ysiw'", { remap = true }, 'Surround word with single quotes')
    nmap("ys'", "ysiw'", { remap = true }, 'Surround word with single quotes')
    nmap('ys"', 'ysiw"', { remap = true }, 'Surround word with double quotes')
    nmap('ys`', 'ysiw`', { remap = true }, 'Surround word with backticks')

    nmap('ysb', 'ysiw(', { remap = true }, 'Surround word with parentheses')
    nmap('ys(', 'ysiw(', { remap = true }, 'Surround word with parentheses')
    nmap('ys{', 'ysiw{', { remap = true }, 'Surround word with braces')
    nmap('ys[', 'ysiw[', { remap = true }, 'Surround word with brackets')

    nmap('yst', 'ysiwt', { remap = true }, 'Surround word with tag')
    -- }}}
    -- mini.trailspace {{{
    require('mini.trailspace').setup()
    nmap('<Leader>W', function()
      if vim.bo.modifiable then
        MiniTrailspace.trim()
      end
    end, 'Trim trailing whitespace')
    -- }}}
  end,
}
