local util = require('util')

local map = util.map
local nmap = util.nmap

return {
  { 'AndrewRadev/splitjoin.vim' },
  { 'arp242/jumpy.vim' },
  { 'numToStr/Comment.nvim', config = true },
  { 'tpope/vim-abolish' },
  { 'tpope/vim-characterize' },
  { 'tpope/vim-endwise' },
  { 'tpope/vim-eunuch' },
  { 'tpope/vim-rails' },
  { 'tpope/vim-repeat' },
  { 'tpope/vim-scriptease' },

  { 'alexghergh/nvim-tmux-navigation',
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
    init = function()
      vim.g.bufferize_focus_output = true
      util.alias_cmd({ B = 'Bufferize' })
    end
  },

  { 'andymass/vim-matchup',
    init = function()
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_offscreen = {}
    end
  },

  { 'kopischke/vim-fetch',
    config = function()
      map({ 'n', 'x' }, 'gF', '<C-w><C-f>', 'Go to file in split')
    end
  },

  { 'NvChad/nvim-colorizer.lua',
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
    opts = {
      color_icons = false,
    },
  },

  { 'ojroques/nvim-bufdel',
    opts = {
      next = 'tabs',
      quit = false,
    },
  },

  { 'psliwka/vim-smoothie',
    init = function()
      vim.g.smoothie_speed_constant_factor = 20
      vim.g.smoothie_speed_linear_factor = 20
    end
  },

  { 'sickill/vim-pasta',
    init = function()
      vim.g.pasta_disabled_filetypes = { 'qf', 'fugitiveblame' }
    end
  },

  { 'stevearc/aerial.nvim',
    init = function()
      -- TODO: implement
      -- nmap('<Leader>t', require('util/fzf-aerial'))
      nmap('<Leader>t', ':AerialNavToggle', 'Toggle symbols in popup')
      nmap('<Leader>T', ':AerialToggle', 'Toggle symbols in sidebar')
    end,
    opts = {
      nav = {
        max_width = 0.2,
        win_opts = {winblend = 5 },
        keymaps = { q = 'actions.close' },
      },
    }
  },

  { 'tpope/vim-dispatch',
    init = function()
      vim.g.dispatch_no_maps = 1
      vim.g.dispatch_handlers = { 'job' }
    end
  },

  { 'tpope/vim-projectionist',
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
    config = function()
      vim.cmd([[
        imap <C-]> </<Plug>ragtagHtmlComplete
        autocmd User Ragtag iunmap <buffer> <C-v>%
        autocmd User Ragtag iunmap <buffer> <C-v>&
      ]])
    end
  },

  { 'tpope/vim-rsi',
    config = function()
      vim.cmd([[
        " restore default mapping for <C-d>
        iunmap <C-d>
        cunmap <C-d>
      ]])
    end
  },
}
