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
        ['<M-e>'] = 'preview-down',
        ['<M-y>'] = 'preview-up',
        ['<M-f>'] = 'preview-half-page-down',
        ['<M-b>'] = 'preview-half-page-up',
        ['<M-r>'] = 'preview-page-reset',
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
      header_bind = 'WarningMsg',
      header_text = 'Type',
      buf_flag_cur = 'Title',
      buf_flag_alt = 'WarningMsg',
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

    oldfiles = {
      include_current_session = true,
      file_ignore_patterns = { '%.git/COMMIT_EDITMSG' },
    },

    tags = {
      ctags_autogen = true,
      cmd = 'ctags -f - $( git ls-files ) 2>/dev/null',
    },

    grep = {
      multiline = 1,
      RIPGREP_CONFIG_PATH = vim.env.RIPGREP_CONFIG_PATH,
    },

    keymaps = {
      formatter = '%s | %-10s | %-30s | %s',
    },

    colorschemes = {
      colors = {
        'nordfox',
        'nightfox',
        'duskfox',
        'carbonfox',
        'gruvbox-material',
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
    local actions = fzf.actions
    local defaults = fzf.defaults

    -- use history per provider
    vim.g.fzf_history_dir = vim.fn.stdpath('state') .. '/fzf'

    -- override actions
    defaults.files.actions = {
      ['ctrl-g'] = actions.toggle_ignore,
      ['alt-i'] = false,
      ['alt-h'] = false,
      ['alt-f'] = false,
    }

    defaults.helptags.actions.default = actions.help_tab
    defaults.actions.files.default = function(selected, settings)
      actions.file_edit(selected, settings)

      if #selected > 1 then
        actions.file_sel_to_qf(selected, settings)
        vim.cmd.wincmd('p')
      end
    end

    -- add directory previewer
    opts.previewers = {
      tree = {
        cmd = 'tree',
        args = '-dxC --gitignore --prune --noreport',
        _ctor = require('fzf-lua.previewer').fzf.cmd,
      },
    }

    -- add default settings
    local reverse = {
      fzf_opts = { ['--layout'] = 'reverse-list', ['--no-sort'] = true },
    }

    local preview = {
      winopts = { preview = { hidden = 'nohidden' }},
      fzf_opts = { ['--layout'] = 'reverse' },
    }

    opts.lines = merge(opts.lines, reverse)
    opts.blines = merge(opts.lines, reverse)

    opts.helptags = merge(opts.helptags, preview)
    opts.highlights = merge(opts.highlights, preview)
    opts.jumps = merge(opts.jumps, preview)

    opts.git = merge(opts.git)
    opts.git.status = merge(opts.git.status, preview)

    opts.lsp = merge(opts.lsp)
    opts.lsp.finder = merge(opts.lsp.finder, preview)
    opts.lsp.code_actions = merge(opts.lsp.code_actions, preview)

    fzf.setup(opts)

    -- map each provider with '<Leader><key>`,
    -- and '<Leader><Leader><key>` for resuming
    local function get_args(args)
      if type(args) == 'function' then
        args = args()
      else
        args = merge({}, args)
      end

      if args.cwd == '' then
        args.cwd = nil
      end

      return args
    end

    local function map_fzf(key, provider, args, name)
      name = name or provider
        :gsub('_', ' ')
        :gsub('git', 'Git')
        :gsub('lsp', 'LSP')

      local handler = provider
      if type(handler) ~= 'function' then
        handler = function()
          fzf[provider](get_args(args))
        end
      end

      nmap(key, handler, 'Search ' .. name)

      nmap('<Leader>' .. key, function()
        local resume_args = get_args(args)
        resume_args.resume = true
        resume_args.query = nil
        fzf[provider](resume_args)
      end, 'Resume ' .. name .. ' search')
    end

    -- resume last provider
    nmap('<Leader><Leader><Leader>', fzf.resume, 'Resume last search')

    -- search files
    map_fzf('<Leader>f', 'files')
    map_fzf('<Leader>F', 'files', function() return { cwd = expand('%:h') } end, 'files in current directory')
    map_fzf('<Leader>o', 'files', { cwd = '/slack/documents/Notes', fd_opts = '-e md' }, 'Obsidian notes')

    map_fzf('<Leader>b', 'buffers')
    map_fzf('<Leader>B', 'buffers', { show_unlisted = true }, 'all buffers')

    map_fzf('<Leader>h', 'oldfiles', nil, 'recent files')
    map_fzf('<Leader>H', 'oldfiles', { cwd_only = true }, 'recent files in current directory')

    map_fzf('<Leader>j', 'jumps')

    -- search file contents
    map_fzf('<Leader>r', 'live_grep', function() return { query = expand('<cword>') } end, 'by regex in project')
    map_fzf('<Leader>R', 'live_grep', function() return { query = expand('<cword>'), cwd = expand('%:h') } end, 'by regex in current directory')

    map_fzf('<Leader>l', 'blines', function() return { query = expand('<cword>') } end, 'lines in buffer')
    map_fzf('<Leader>L', 'lines', function() return { query = expand('<cword>') } end, 'lines in all buffers')

    map_fzf('<Leader>t', 'btags', nil, 'buffer symbols')
    map_fzf('<Leader>T', 'tags', nil, 'project symbols')

    -- search vim history
    map_fzf('<Leader>:', 'command_history')
    map_fzf('<Leader>/', 'search_history')

    -- search vim internals
    map_fzf('<F1><F1>', 'help_tags')
    map_fzf('<F1>m', 'man_pages')
    map_fzf('<F1>k', 'keymaps')
    map_fzf('<F1>c', 'commands')
    map_fzf('<F1>h', 'highlights')
    map_fzf('<F1>C', 'colorschemes')

    -- search spellcheck
    map_fzf('<Leader>z', 'spell_suggest', nil, 'spelling suggestions')

    -- search LSP
    map_fzf('<Leader>da', 'lsp_code_actions')
    map_fzf('<Leader>dd', 'lsp_definitions')
    map_fzf('<Leader>dD', 'lsp_declarations')
    map_fzf('<Leader>de', 'lsp_document_diagnostics')
    map_fzf('<Leader>dE', 'lsp_workspace_diagnostics')
    map_fzf('<Leader>di', 'lsp_implementations')
    map_fzf('<Leader>dI', 'lsp_incoming_calls')
    map_fzf('<Leader>dO', 'lsp_outgoing_calls')
    map_fzf('<Leader>dr', 'lsp_references')
    map_fzf('<Leader>ds', 'lsp_document_symbols')
    map_fzf('<Leader>dS', 'lsp_live_workspace_symbols')
    map_fzf('<Leader>dt', 'lsp_typedefs')

    -- search git
    map_fzf('<Leader>gm', 'git_status')
    map_fzf('<Leader>gc', 'git_branches')

    -- search projects
    local projects = function(settings)
      settings = merge(settings, {
        fd_opts = '-u --glob --type d ".{git,obsidian}" ~/src /slack',
        toggle_hidden_flag = '--exclude "{archive,packages}"',
        fzf_opts = { ['--multi'] = false },
        previewer = 'tree',
        cwd_prompt = false,

        winopts = {
          title = ' Projects ',
          height = 12,
          row = 0.85,
          preview = { layout = 'horizontal' },
        },

        fn_transform = function(path)
          path = path:gsub('/%.[^/]*/$', '')
          return string.format('%-32s%s%s',
            fzf.utils.ansi_codes.cyan('󰂿 ' .. vim.fn.fnamemodify(path, ':t')),
            fzf.utils.nbsp,
            fzf.utils.ansi_codes.blue(vim.fn.fnamemodify(path, ':~'))
          ), path
        end,

        actions = {
          ['ctrl-g'] = actions.toggle_hidden,
          ['default'] = function(selected)
            if util.buffer_count() > 0 then
              vim.cmd.tabnew()
            end

            local file = fzf.path.entry_to_file(selected[1])
            vim.cmd.lcd(file.path)
            fzf.files({ cwd = file.path })
          end
        }
      })

      fzf.files(settings)
    end

    map_fzf('<Leader>gp', projects, nil, 'projects')
  end
}
