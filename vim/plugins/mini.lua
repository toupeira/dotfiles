local util = require('util')

local nmap = util.nmap
local vmap = util.vmap
local imap = util.imap

return {
  'echasnovski/mini.nvim',

  config = function()
    require('mini.ai').setup()
    require('mini.surround').setup()

    require('mini.align').setup({
      mappings = {
        start = '<Leader>a',
        start_with_preview = '<Leader>A',
      },
    })

    require('mini.basics').setup({
      options      = { basic = false, win_borders = 'bold' },
      mappings     = { basic = false, move_with_alt = true },
      autocommands = { basic = true },
    })

    require('mini.bracketed').setup({
      diagnostic = { suffix = 'e' },

      comment    = { suffix = '' },
      file       = { suffix = '' },
      treesitter = { suffix = '' },
    })

    require('mini.files').setup({
      mappings = {
        go_in = 'L',
        go_in_plus = 'l',
        go_out = 'H',
        go_out_plus = 'h',
        synchronize = '!',
      },

      windows = {
        width_focus = 30,
      },
    })
    nmap('-', ':lua MiniFiles.open()', 'Open file explorer')

    require('mini.misc').setup_auto_root(
      { '.git' }, vim.fs.dirname
    )

    require('mini.move').setup()
    imap('<M-H>', ':lua MiniMove.move_line("left")', 'Move line left')
    imap('<M-J>', ':lua MiniMove.move_line("down")', 'Move line down')
    imap('<M-K>', ':lua MiniMove.move_line("up")', 'Move line up')
    imap('<M-L>', ':lua MiniMove.move_line("right")', 'Move line right')

    require('mini.operators').setup({
      sort = { prefix = '' },
    })
    vmap('D', 'gm', { remap = true }, 'Duplicate selection')

    require('mini.pairs').setup({
      modes = {
        command = false,
        terminal = false,
      },

      mappings = {
        ['('] = { neigh_pattern = '[^\\][ \n]' },
        ['['] = { neigh_pattern = '[^\\][ \n]' },
        ['{'] = { neigh_pattern = '[^\\][ \n]' },
        ['"'] = { neigh_pattern = '[^\\][ \n]' },
        ["'"] = { neigh_pattern = '[^%a\\][ \n]' },
        ['`'] = { neigh_pattern = '[^\\][ \n]' },

        [' '] = {
          action = 'closeopen',
          pair = '  ',
          neigh_pattern = '[%(%[{][%)%]}]',
        },
      }
    })
    -- util.unmap('c', '<Space>') -- clashes with cabbrev

    local starter = require('mini.starter')
    local is_home = vim.fn.getcwd() == os.getenv('HOME')

    util.command('MiniStarter', 'lua MiniStarter.open()', 'Open start screen')

    starter.setup({
      evaluate_single = true,

      items = {
        { section = 'Builtin actions', name = 'Edit new file', action = 'enew' },
        { section = 'Builtin actions', name = 'Insert mode', action = function () vim.cmd.enew(); vim.cmd.startinsert() end },
        { section = 'Builtin actions', name = 'Quit', action = 'quitall' },

        not is_home and starter.sections.recent_files(9, true, true),

        { section = 'Bookmarks', name = 'vimrc', action = 'edit ~/.config/nvim/init.lua' },
        { section = 'Bookmarks', name = 'gitconfig', action = 'edit ~/.config/git/config' },
        { section = 'Bookmarks', name = 'tmux.conf', action = 'edit ~/.config/tmux/tmux.conf' },
      },

      footer = '',

      content_hooks = {
        starter.gen_hook.adding_bullet(),
        starter.gen_hook.indexing('all', { 'Builtin actions', 'Bookmarks' }),
        starter.gen_hook.aligning('center', 'top'),
        starter.gen_hook.padding(0, vim.o.lines / (is_home and 3 or 5)),
      },
    })

    require('mini.trailspace').setup()
    nmap('<Leader>$', ':lua MiniTrailspace.trim()', 'Trim trailing whitespace')
    util.hl_link('MiniTrailspace', 'Visual')
  end
}
