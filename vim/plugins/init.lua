local util = require('util')

local lazy = util.lazy
local lazy_file = util.lazy_file
local map = util.map
local nmap = util.nmap

return {
  lazy { 'j-hui/fidget.nvim', config = true },
  lazy { 'tpope/vim-characterize' },
  lazy { 'tpope/vim-eunuch' },
  lazy { 'tpope/vim-repeat' },
  lazy { 'tpope/vim-scriptease' },

  lazy_file { 'AndrewRadev/splitjoin.vim' },
  lazy_file { 'arp242/jumpy.vim' },
  lazy_file { 'numToStr/Comment.nvim', config = true },
  lazy_file { 'tiagovla/scope.nvim', config = true },
  lazy_file { 'tpope/vim-abolish' },

  { 'alexghergh/nvim-tmux-navigation',
    event = 'VeryLazy',
    cond = function()
      return os.getenv('TMUX')
    end,
    opts = {
      disable_when_zoomed = true,
      map_modes = { 'n', 't' },
      keybindings = {
        left = '<C-h>',
        down = '<C-j>',
        up = '<C-k>',
        right = '<C-l>',
      }
    }
  },

  { 'AndrewRadev/bufferize.vim',
    cmd = 'Bufferize',
    init = function()
      vim.g.bufferize_focus_output = true
      util.alias_cmd({ B = 'Bufferize' })
    end
  },

  { 'andymass/vim-matchup',
    event = 'LazyFile',
    init = function()
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_offscreen = {}
    end
  },

  { 'kopischke/vim-fetch',
    event = 'LazyFile',
    config = function()
      map({ 'n', 'x' }, 'gF', '<C-w><C-f>', 'Go to file in split')
    end
  },

  { 'NvChad/nvim-colorizer.lua',
    event = 'LazyFile',
    init = function()
      util.alias_cmd({ CT = 'ColorizerToggle' })
    end,
    opts = {
      filetypes = { 'css', 'lua', 'scss' },
      user_default_options = {
        names = false,
      },
    },
  },

  { 'nvim-tree/nvim-web-devicons',
    lazy = true,
    cond = vim.o.termguicolors,
    opts = {
      color_icons = false,
      default = true,
    }
  },

  { 'ojroques/nvim-bufdel',
    cmd = 'BufDel',
    opts = {
      next = 'tabs',
      quit = false,
    },
    init = function ()
      nmap('<Leader>x', { 'lclose', 'BufDel' }, 'Close current buffer (keep window)')
    end
  },

  { 'psliwka/vim-smoothie',
    event = 'VeryLazy',
    init = function()
      vim.g.smoothie_speed_constant_factor = 20
      vim.g.smoothie_speed_linear_factor = 20
    end
  },

  { 'sickill/vim-pasta',
    event = 'VeryLazy',
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

  { 'tpope/vim-projectionist',
    event = 'VeryLazy',
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

  { 'tpope/vim-ragtag',
    event = 'LazyFile',
    config = function()
      vim.cmd([[
        imap <C-]> </<Plug>ragtagHtmlComplete
        autocmd User Ragtag iunmap <buffer> <C-v>%
        autocmd User Ragtag iunmap <buffer> <C-v>&
      ]])
    end
  },

  { 'tpope/vim-rsi',
    event = 'VeryLazy',
    config = function()
      vim.cmd([[
        " restore default mapping for <C-d>
        iunmap <C-d>
        cunmap <C-d>
      ]])
    end
  },
}
