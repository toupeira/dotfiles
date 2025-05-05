local util = require('util')
local very_lazy = util.very_lazy
local lazy_file = util.lazy_file
local map = util.map
local imap = util.imap

return {
  { 'nvim-lua/plenary.nvim', lazy = true },

  lazy_file { 'AndrewRadev/splitjoin.vim' },
  lazy_file { 'tpope/vim-rails' },

  very_lazy { 'j-hui/fidget.nvim', config = true },
  very_lazy { 'numToStr/Comment.nvim', config = true },
  very_lazy { 'tiagovla/scope.nvim', config = true },
  very_lazy { 'tpope/vim-abolish' },
  very_lazy { 'tpope/vim-characterize' },
  very_lazy { 'tpope/vim-repeat' },
  very_lazy { 'tpope/vim-scriptease' },

  { 'AndrewRadev/bufferize.vim',
    cmd = 'Bufferize',
    init = function()
      vim.g.bufferize_focus_output = true
      util.alias_command({ B = 'Bufferize' })
    end
  },

  lazy_file { 'andymass/vim-matchup',
    init = function()
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_offscreen = {}
    end
  },

  { 'chrisgrieser/nvim-spider',
    keys = {
      { 'w', '<Cmd>lua require("spider").motion("w")<CR>', mode = { 'n', 'o', 'x' }},
      { 'cw', 'ce', mode = 'n', { remap = true }},
      { 'e', '<Cmd>lua require("spider").motion("e")<CR>', mode = { 'n', 'o', 'x' }},
      { 'b', '<Cmd>lua require("spider").motion("b")<CR>', mode = { 'n', 'o', 'x' }},
    },
  },

  lazy_file { 'mong8se/actually.nvim',
    dependencies = { 'dressing.nvim' },
  },

  { 'NvChad/nvim-colorizer.lua',
    ft = { 'css', 'scss', 'lua' },
    cmd = 'ColorizerToggle',
    init = function()
      util.alias_command({ CT = 'ColorizerToggle' })
    end,
    opts = function(plugin)
      return {
        filetypes = plugin.ft,
        user_default_options = {
          names = false,
        },
      }
    end
  },

  { 'pechorin/any-jump.vim',
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

      util.autocmd('FileType', 'any-jump', function()
        vim.bo.buflisted = false
      end)
    end
  },

  very_lazy { 'psliwka/vim-smoothie',
    init = function()
      vim.g.smoothie_speed_constant_factor = 20
      vim.g.smoothie_speed_linear_factor = 20
    end
  },

  lazy_file { 'sickill/vim-pasta',
    init = function()
      vim.g.pasta_disabled_filetypes = { 'qf', 'fugitiveblame' }
    end
  },

  lazy_file { 'sphamba/smear-cursor.nvim',
    opts = {
      smear_insert_mode = false,
      min_horizontal_distance_smear = 10,
      min_vertical_distance_smear = 2,
    },
  },

  lazy_file { 'stevearc/dressing.nvim',
    dependencies = { 'fzf-lua' },
    opts = {
      input = { enabled = true },
      select = { enabled = true, backend = 'fzf_lua' },
    },
  },

  { 'tpope/vim-dispatch',
    cmd = { 'Dispatch', 'Make' },
    init = function()
      vim.g.dispatch_no_maps = 1
      vim.g.dispatch_handlers = { 'job' }
    end
  },

  lazy_file { 'tpope/vim-eunuch',
    init = function()
      util.alias_command({
        ['D'] = 'Delete',
        ['D!'] = 'Delete!'
      })
    end
  },

  lazy_file { 'tpope/vim-projectionist',
    config = function()
      local function alternate_create()
        local confirm = vim.o.confirm
        local ok, _ = pcall(function() vim.cmd.A() end)

        if not ok then
          util.echo('No alternate file', 'ErrorMsg')
        end

        vim.o.confirm = confirm
      end

      vim.api.nvim_create_user_command('AC', alternate_create, {})
    end
  },

  very_lazy { 'tpope/vim-ragtag',
    config = function()
      imap('<C-]>', '</<Plug>ragtagHtmlComplete')
      util.autocmd('User', 'Ragtag', function()
        util.unmap('i', '<C-v>%', { buffer = true })
        util.unmap('i', '<C-v>&', { buffer = true })
      end)
    end
  },

  very_lazy { 'tpope/vim-rsi',
    config = function()
      -- restore default mapping for <C-d>
      util.unmap({ 'i', 'c' }, '<C-d>')

      -- use nvim-spider for word movements
      imap('<M-f>', '<Esc>l<Cmd>lua require("spider").motion("w")<CR>i', { force = true })
      imap('<M-b>', '<Esc><Cmd>lua require("spider").motion("b")<CR>i', { force = true })
    end
  },

  lazy_file { 'wsdjeg/vim-fetch',
    config = function()
      map({ 'n', 'x' }, 'gF', '<C-w><C-f>', { force = true }, 'Go to file in split')
    end
  },

  {
    'y3owk1n/time-machine.nvim',
    cmd = 'TimeMachineToggle',
    keys = {
      { '<Leader>u', '<Cmd>TimeMachineToggle<CR>', desc = 'Toggle time machine' },
    },
    opts = {
      split_opts = {
        split = 'right',
        width = 35,
      },
    }
  },

  very_lazy { 'ziontee113/icon-picker.nvim',
    keys = {
      { '<M-.>', '<Cmd>IconPickerInsert<CR>', mode = { 'i' }, desc = 'Insert emoji' },
    },
    opts = { disable_legacy_commands = true },
  },
}
