local util = require('util')
local lazy = util.lazy
local very_lazy = util.very_lazy
local lazy_file = util.lazy_file
local map = util.map
local imap = util.imap

return {
  lazy_file { 'AndrewRadev/splitjoin.vim', submodules = false },
  lazy_file { 'mong8se/actually.nvim' },
  lazy_file { 'tpope/vim-rails' },

  very_lazy { 'tiagovla/scope.nvim', config = true },
  very_lazy { 'tpope/vim-abolish' },
  very_lazy { 'tpope/vim-characterize' },
  very_lazy { 'tpope/vim-repeat' },
  very_lazy { 'tpope/vim-scriptease' },

  lazy      { 'AndrewRadev/bufferize.vim',
    cmd = 'Bufferize',
    init = function()
      vim.g.bufferize_focus_output = true
      util.alias_command({ B = 'Bufferize' })
    end,
  },
  lazy_file { 'andymass/vim-matchup',
    init = function()
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_offscreen = {}
    end,
  },
  lazy      { 'folke/flash.nvim',
    keys = {
      { '<Leader>j', mode = { 'n', 'x', 'o' }, function() require('flash').jump() end, desc = 'Jump to position' },
      { '<Leader>J', mode = { 'n', 'x', 'o' }, function() require('flash').treesitter() end, desc = 'Select Treesitter node' },
      { 'r', mode = 'o', function() require('flash').remote() end, desc = 'Remote jump to position' },
      { 'R', mode = { 'o', 'x' }, function() require('flash').treesitter_search() end, desc = 'Remote select Treesitter node' },
    },
    opts = {
      modes = {
        search = { enabled = false },
        char = { enabled = false },
      },
    },
  },
  very_lazy { 'j-hui/fidget.nvim',
    keys = {
      { '<Leader>N', '<Cmd>Bufferize Fidget history<CR>', 'Show notification history' },
    },

    opts = {
      notification = {
        view = { group_separator = '——' },
      },
    },

    init = function()
      local fidget = require('fidget')
      fidget.notification.default_config.name = ' '
      fidget.notification.default_config.ttl = 2

      local notify = vim.notify
      vim.notify = function(msg, level, opts)
        if type(level) == 'number' and level >= vim.log.levels.ERROR then
          return notify(msg, level, opts)
        else
          return fidget.notify(msg, level, opts)
        end
      end
    end,
  },
  lazy      { 'pechorin/any-jump.vim',
    url = 'https://github.com/toupeira/any-jump.vim',
    branch = 'feat/window-borders',
    keys = {
      { 'gd', '<Cmd>AnyJump<CR>', desc = 'Jump to definition' },
      { 'gd', '<Cmd>AnyJumpVisual<CR>', mode = { 'v' }, desc = 'Jump to definition' },
      { 'gD', 'gd', desc = 'Go to definition' },
    },
    init = function()
      vim.g.any_jump_disable_default_keybindings = 1
      vim.g.any_jump_center_screen_after_jump = true
      vim.g.any_jump_window_border = 'rounded'
      vim.g.any_jump_window_width_ratio = 0.8

      util.autocmd('FileType', 'any-jump', function()
        vim.bo.buflisted = false
      end)
    end,
  },
  very_lazy { 'psliwka/vim-smoothie',
    init = function()
      vim.g.smoothie_speed_constant_factor = 20
      vim.g.smoothie_speed_linear_factor = 20
    end,
  },
  lazy_file { 'sickill/vim-pasta',
    init = function()
      vim.g.pasta_disabled_filetypes = { 'qf', 'fugitiveblame' }
    end,
  },
  very_lazy { 'sphamba/smear-cursor.nvim',
    keys = {
      { '<LocalLeader>t', function()
        local cursor = require('smear_cursor')
        cursor.toggle()
        util.notify_toggle('Cursor trail:', cursor.enabled)
      end, mode = { 'n' }, desc = 'Toggle cursor trail' },
    },
    opts = {
      enabled = false,
      smear_insert_mode = false,
      min_horizontal_distance_smear = 10,
      min_vertical_distance_smear = 2,
    },
  },
  lazy_file { 'tpope/vim-eunuch',
    init = function()
      util.alias_command({
        ['D'] = 'Delete',
        ['D!'] = 'Delete!'
      })
    end,
  },
  lazy_file { 'tpope/vim-projectionist',
    config = function()
      util.command('AC', function()
        local confirm = vim.o.confirm
        local ok, _ = pcall(function() vim.cmd.A() end)

        if not ok then
          util.error('No alternate file')
        end

        vim.o.confirm = confirm
      end, 'Create alternate file')
    end,
  },
  very_lazy { 'tpope/vim-ragtag',
    config = function()
      imap('<C-]>', '</<Plug>ragtagHtmlComplete')
      util.autocmd('User', 'Ragtag', function()
        util.unmap('i', '<C-v>%', { buffer = true })
        util.unmap('i', '<C-v>&', { buffer = true })
      end)
    end,
  },
  very_lazy { 'tpope/vim-rsi',
    config = function()
      -- restore default mapping for <C-d>
      util.unmap({ 'i', 'c' }, '<C-d>')
    end,
  },
  lazy_file { 'willothy/flatten.nvim',
    opts = {
      window = { open = 'alternate' },
    },
  },
  lazy_file { 'wsdjeg/vim-fetch',
    config = function()
      map('n', 'gf', '<Cmd>call fetch#cfile(v:count1)<CR>', 'Go to file')
      map('v', 'gf', '<Cmd>call fetch#visual(v:count1)<CR>', 'Go to file')

      map('n', 'gF', '<C-w><C-s><Cmd>call fetch#cfile(v:count1)<CR>', { force = true }, 'Go to file in split')
      map('v', 'gF', '<C-w><C-s><Cmd>call fetch#visual(v:count1)<CR>', { force = true }, 'Go to file in split')
    end,
  },
  lazy      { 'ziontee113/icon-picker.nvim',
    keys = {
      { '<M-.>', '<Cmd>IconPickerInsert<CR>', mode = { 'i' }, desc = 'Insert emoji' },
    },
    opts = { disable_legacy_commands = true },
  },
}
