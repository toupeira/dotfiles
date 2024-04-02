local util = require('util')

local nmap = util.nmap
local merge = util.merge

local expand = vim.fn.expand

return {
  'ibhagwan/fzf-lua',
  lazy = false,

  config = function()
    local fzf = require('fzf-lua')
    local actions = require('fzf-lua.actions')

    -- keymaps
    -- map each provider with ',<key>` and ',,<key>` for resuming
    local function get_args(args)
      if type(args) == 'function' then return args() end
      if args then return merge({}, args) end
      return {}
    end

    local function map_fzf(key, provider, args)
      nmap(key, function()
        fzf[provider](get_args(args))
      end, 'Search ' .. provider)

      nmap('<Leader>' .. key, function()
        local resume_args = get_args(args)
        resume_args.resume = true
        resume_args.query = nil
        fzf[provider](resume_args)
      end, 'Resume last ' .. provider .. ' search')
    end

    -- resume last provider
    nmap('<Leader>,,', fzf.resume, 'Resume last search')

    -- files
    map_fzf('<Leader>f', 'files')
    map_fzf('<Leader>F', 'files', function() return expand('%') ~= '' and { cwd = expand('%:h') } end)
    map_fzf('<Leader>h', 'oldfiles')
    map_fzf('<Leader>H', 'oldfiles', function() return expand('%') ~= '' and { cwd_only = true } end)
    map_fzf('<Leader>b', 'buffers')

    -- file contents
    map_fzf('<Leader>r', 'grep_cword')
    map_fzf('<Leader>R', 'live_grep')
    map_fzf('<Leader>l', 'blines', function() return { query = expand('<cword>') } end)
    map_fzf('<Leader>L', 'lines', function() return { query = expand('<cword>') } end)

    -- vim history
    map_fzf('<Leader>:', 'command_history')
    map_fzf('<Leader>/', 'search_history')

    -- vim internals
    map_fzf('<Leader>??', 'help_tags')
    map_fzf('<Leader>?m', 'man_pages')
    map_fzf('<Leader>?k', 'keymaps')
    map_fzf('<Leader>?c', 'commands')
    map_fzf('<Leader>?h', 'highlights')
    map_fzf('<Leader>?C', 'colorschemes')

    -- spellcheck
    map_fzf('<Leader>z', 'spell_suggest')

    -- git
    map_fzf('<Leader>gm', 'git_status')
    map_fzf('<Leader>gB', 'git_branches')

    -- default settings
    local opts = {
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

      fzf_opts = {
        ['--layout'] = 'default',
      },

      hls = {
        border = 'NonText',
        preview_border = 'NonText',
        help_border = 'NonText',
      },

      actions = {
        files = { default = actions.file_edit },
      },

      files = {
        fd_opts = '--color always --max-results 99999 --type f --type l --hidden --exclude .git',
        color_icons = false,
        git_icons = false,
        no_header = true,
      },

      buffers = {
        winopts = {
          height = 12,
          row = 0.85,
        },
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
    }

    local preview = { winopts = { preview = { hidden = 'nohidden' }}}
    local reverse = { fzf_opts = { ['--layout'] = 'reverse-list', ['--no-sort'] = true }}

    opts.lines = merge(opts.lines, reverse)
    opts.blines = merge(opts.lines, reverse)

    opts.helptags = merge(opts.helptags, preview)
    opts.highlights = merge(opts.highlights, preview)

    opts.git = merge(opts.git, {})
    opts.git.status = merge(opts.git.status, preview)

    fzf.setup(opts)
  end
}
