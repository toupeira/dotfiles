local util = require('util')
local very_lazy = util.very_lazy
local lazy_file = util.lazy_file
local map = util.map
local imap = util.imap

return {
  { 'nvim-lua/plenary.nvim', lazy = true },
  { 'nvim-tree/nvim-web-devicons', lazy = true,
    opts = { color_icons = false, default = true }},

  very_lazy { 'j-hui/fidget.nvim', config = true },
  very_lazy { 'numToStr/Comment.nvim', config = true },
  very_lazy { 'tiagovla/scope.nvim', config = true },
  very_lazy { 'tpope/vim-abolish' },
  very_lazy { 'tpope/vim-characterize' },
  very_lazy { 'tpope/vim-eunuch' },
  very_lazy { 'tpope/vim-repeat' },
  very_lazy { 'tpope/vim-scriptease' },

  { 'AndrewRadev/splitjoin.vim' },

  { 'AndrewRadev/bufferize.vim',
    cmd = 'Bufferize',
    init = function()
      vim.g.bufferize_focus_output = true
      util.alias_cmd({ B = 'Bufferize' })
    end
  },

  lazy_file { 'andymass/vim-matchup',
    init = function()
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_offscreen = {}
    end
  },

  {
    'chrishrb/gx.nvim',
    config = true,
    cmd = { 'Browse' },
    keys = {
      { 'gx', '<Cmd>Browse<CR>', mode = { 'n', 'x' } },
    },
  },

  lazy_file { 'kopischke/vim-fetch',
    config = function()
      map({ 'n', 'x' }, 'gF', '<C-w><C-f>', { force = true }, 'Go to file in split')
    end
  },

  { 'NvChad/nvim-colorizer.lua',
    ft = { 'css', 'scss', 'lua' },
    cmd = 'ColorizerToggle',
    init = function()
      util.alias_cmd({ CT = 'ColorizerToggle' })
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
      { 'gD', '<Cmd>AnyJumpLastResults<CR>', desc = 'Resume jump to definition' },
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

  { 'tpope/vim-dispatch',
    cmd = { 'Dispatch', 'Make' },
    init = function()
      vim.g.dispatch_no_maps = 1
      vim.g.dispatch_handlers = { 'job' }
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
    end
  },
}
