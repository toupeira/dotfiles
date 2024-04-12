local util = require('util')

local nmap = util.nmap
local merge = util.merge
local expand = vim.fn.expand

return {
  'ibhagwan/fzf-lua',
  event = 'VeryLazy',

  opts = {
    winopts = {
      width = 0.9,
      height = 0.9,
      preview = {
        hidden = 'hidden',
        vertical = 'up:60%',
        horizontal = 'right:50%',
        title = false,
      },
    },

    keymap = {
      builtin = {
        ['<F1>']  = 'toggle-help',
        ['<C-_>'] = 'toggle-preview',
        ['<C-e>'] = 'preview-page-down',
        ['<C-y>'] = 'preview-page-up',
        ['<C-f>'] = 'preview-page-down',
        ['<C-b>'] = 'preview-page-up',
      },
      -- remove defaults, still uses $FZF_DEFAULT_OPTS
      fzf = {},
    },

    hls = {
      border = 'NonText',
      preview_border = 'NonText',
      help_border = 'NonText',
      header_bind = 'WarningMsg',
      header_text = 'Type',
      buf_flag_cur = 'Title',
      buf_flag_alt = 'WarningMsg',
    },

    fzf_opts = {
      ['--layout'] = 'default',
    },

    defaults = {
      color_icons = false,
    },

    files = {
      fd_opts = '--color always --max-results 99999 --type f --type l --hidden --exclude .git',
      git_icons = false,
      no_header = true,
    },

    buffers = {
      winopts = { height = 12, row = 0.85 },
      fzf_opts = { ['--header-lines'] = false },
    },

    colorschemes = {
      colors = {
        'nordfox',
        'nightfox',
        'duskfox',
        'carbonfox',
        'catppuccin-mocha',
        'material-deep-ocean',
        'tokyonight-night',

        'dayfox',
        'dawnfox',
        'catppuccin-latte',
        'material-lighter',
        'tokyonight-day',
      },
    },
  },

  config = function(_, opts)
    local fzf = require('fzf-lua')
    local actions = require('fzf-lua.actions')

    -- override actions
    opts.actions = {
      files = { default = actions.file_edit },
      helptags = { default = actions.help_tab },
    }

    -- add default settings
    local preview = { winopts = { preview = { hidden = 'nohidden' }}}
    local reverse = { fzf_opts = { ['--layout'] = 'reverse-list', ['--no-sort'] = true }}

    opts.lines = merge(opts.lines, reverse)
    opts.blines = merge(opts.lines, reverse)

    opts.helptags = merge(opts.helptags, preview)
    opts.highlights = merge(opts.highlights, preview)

    opts.git = merge(opts.git, {})
    opts.git.status = merge(opts.git.status, preview)

    fzf.setup(opts)

    -- map each provider with '<Leader><key>`,
    -- and '<Leader><Leader><key>` for resuming
    local function get_args(args)
      if type(args) == 'function' then return args() end
      if args then return merge({}, args) end
      return {}
    end

    local function map_fzf(key, provider, args)
      local name = provider:gsub('_', ' ')

      nmap(key, function()
        fzf[provider](get_args(args))
      end, 'Search ' .. name)

      nmap('<Leader>' .. key, function()
        local resume_args = get_args(args)
        resume_args.resume = true
        resume_args.query = nil
        fzf[provider](resume_args)
      end, 'Resume last ' .. name .. ' search')
    end

    -- resume last provider
    nmap('<Leader><Leader><Leader>', fzf.resume, 'Resume last search')

    -- files
    map_fzf('<Leader>f', 'files')
    map_fzf('<Leader>F', 'files', function() return expand('%') ~= '' and { cwd = expand('%:h') } end)
    map_fzf('<Leader>h', 'oldfiles')
    map_fzf('<Leader>H', 'oldfiles', function() return expand('%') ~= '' and { cwd_only = true } end)
    map_fzf('<Leader>b', 'buffers')

    -- file contents
    map_fzf('<Leader>r', 'live_grep')
    map_fzf('<Leader>R', 'grep_cword')
    map_fzf('<Leader>l', 'blines', function() return { query = expand('<cword>') } end)
    map_fzf('<Leader>L', 'lines', function() return { query = expand('<cword>') } end)

    -- vim history
    map_fzf('<Leader>:', 'command_history')
    map_fzf('<Leader>/', 'search_history')

    -- vim internals
    map_fzf('<F1><F1>', 'help_tags')
    map_fzf('<F1>m', 'man_pages')
    map_fzf('<F1>k', 'keymaps')
    map_fzf('<F1>c', 'commands')
    map_fzf('<F1>h', 'highlights')
    map_fzf('<F1>C', 'colorschemes')

    -- spellcheck
    map_fzf('<Leader>z', 'spell_suggest')

    -- git
    map_fzf('<Leader>gm', 'git_status')
    map_fzf('<Leader>gB', 'git_branches')
  end
}
