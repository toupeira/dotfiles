local util = require('util')
local merge = util.merge

local expand = vim.fn.expand

return {
  'ibhagwan/fzf-lua',
  event = 'VeryLazy',

  opts = {
    winopts = {
      width = 0.9,
      height = 0.9,
      row = 0.5,
      preview = {
        hidden = 'hidden',
        vertical = 'up:60%',
        horizontal = 'right:50%',
        title = false,
      },
    },

    keymap = {
      builtin = {
        true, -- inherit defaults
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
      winopts = { height = 12, row = 0.9 },
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
    opts.builtin = merge(opts.lines, reverse)
    opts.colorschemes = merge(opts.lines, reverse)

    opts.marks = merge(opts.marks, preview)
    opts.jumps = merge(opts.jumps, preview)
    opts.helptags = merge(opts.helptags, preview)
    opts.manpages = merge(opts.manpages, preview)
    opts.highlights = merge(opts.highlights, preview)

    opts.git = merge(opts.git)
    opts.git.branches = merge(opts.git.branches, preview)
    opts.git.diff = merge(opts.git.diff, preview)
    opts.git.status = merge(opts.git.status, preview)

    opts.lsp = merge(opts.lsp)
    opts.lsp.finder = merge(opts.lsp.finder, preview)
    opts.lsp.code_actions = merge(opts.lsp.code_actions, preview)

    fzf.setup(opts)
    require('fzf-lua.providers.ui_select').register()

    -- add mapping to insert last <cword>
    local last_cword
    util.tmap('<M-.>', function()
      if vim.bo.filetype == 'fzf' and last_cword then
        vim.api.nvim_feedkeys(last_cword, 'n', true)
      else
        return '<M-.>'
      end
    end, { expr = true }, 'Insert last <cword>')

    -- helper to generate action arguments
    local function get_args(args)
      args = type(args) == 'function' and args() or merge({}, args)
      args.cwd = args.cwd == '' and nil or args.cwd

      return args
    end

    -- helper to build title options
    local function title(value)
      return { title = ' ' .. value .. ' ' }
    end

    -- helper to add mappings for a provider
    local function map_fzf(key, provider, opts)
      opts = opts or {}
      opts.mode = opts.mode or 'n'
      opts.desc = opts.desc or provider
        :gsub('_', ' ')
        :gsub('git', 'Git')
        :gsub('lsp', 'LSP')

      -- start search with '<Leader><key>'
      util.map(opts.mode, key, function()
        last_cword = expand('<cword>')
        if type(provider) == 'function' then
          provider()
        else
          fzf[provider](get_args(opts.args))
        end
      end, 'Search ' .. opts.desc)

      -- resume last search with '<Leader><Leader><key>'
      util.map(opts.mode, '<Leader>' .. key, function()
        local resume_args = get_args(opts.args)
        resume_args.resume = true
        resume_args.query = nil
        fzf[provider](resume_args)
      end, 'Resume ' .. opts.desc .. ' search')
    end

    -- resume last provider
    util.nmap('<Leader><Leader><Leader>', fzf.resume, 'Resume last search')

    -- search files
    map_fzf('<Leader>f', 'files')
    map_fzf('<Leader>F', 'files', {
      desc = 'files in current directory',
      args = function() return {
        cwd = expand('%:h'),
        winopts = title('Files (current directory)'),
      } end,
    })
    map_fzf('<Leader>o', 'files', {
      desc = 'Obsidian notes',
      args = { cwd = '/slack/documents/Notes', fd_opts = '-e md', winopts = title('Notes') },
    })

    map_fzf('<Leader>b', 'buffers')
    map_fzf('<Leader>B', 'buffers', {
      desc = 'all buffers',
      args = { show_unlisted = true, winopts = title('Buffers (all)') },
    })

    map_fzf('<Leader>h', 'oldfiles', {
      desc = 'recent files',
      args = { winopts = title('Recent files') },
    })
    map_fzf('<Leader>H', 'oldfiles', {
      desc = 'recent files in current directory',
      args = { cwd_only = true, winopts = title('Recent files (current directory)') },
    })

    -- search file contents
    map_fzf('<Leader>r', 'live_grep', { desc = 'by regex in project' })
    map_fzf('<Leader>R', 'live_grep', {
      desc = 'by regex in current directory',
      args = function() return {
        cwd = expand('%:h'),
        winopts = title('Grep (current directory)'),
      } end,
    })

    map_fzf('<Leader>l', 'blines', { desc = 'lines in buffer' })
    map_fzf('<Leader>L', 'lines', { desc = 'lines in all buffers' })

    map_fzf('<Leader>t', 'btags', { desc = 'buffer symbols' })
    map_fzf('<Leader>T', 'tags', { desc = 'project symbols' })

    -- search vim history
    map_fzf('<Leader>:', 'command_history', { mode = { 'n', 'v' }})
    map_fzf('<Leader>/', 'search_history')
    map_fzf('<Leader>"', 'registers')
    map_fzf('<Leader>\'', 'marks')
    map_fzf('<Leader>j', 'jumps')

    -- search vim internals
    map_fzf('<F1><F1>', 'help_tags')
    map_fzf('<F1>m', 'man_pages')
    map_fzf('<F1>k', 'keymaps')
    map_fzf('<F1>c', 'commands')
    map_fzf('<F1>h', 'highlights')
    map_fzf('<F1>C', 'colorschemes')
    map_fzf('<F1>f', 'builtin', { desc = 'FZF providers' })

    -- search spellcheck
    map_fzf('<Leader>z', 'spell_suggest', { desc = 'spelling suggestions' })

    -- search LSP
    map_fzf('<Leader>Sa', 'lsp_code_actions')
    map_fzf('<Leader>Sd', 'lsp_definitions')
    -- map_fzf('<Leader>SD', 'lsp_declarations')
    map_fzf('<Leader>Se', 'lsp_document_diagnostics')
    map_fzf('<Leader>SE', 'lsp_workspace_diagnostics')
    -- map_fzf('<Leader>Si', 'lsp_implementations')
    map_fzf('<Leader>SI', 'lsp_incoming_calls')
    map_fzf('<Leader>SO', 'lsp_outgoing_calls')
    map_fzf('<Leader>Sr', 'lsp_references')
    map_fzf('<Leader>St', 'lsp_document_symbols')
    map_fzf('<Leader>ST', 'lsp_live_workspace_symbols')
    -- map_fzf('<Leader>St', 'lsp_typedefs')

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
          row = 0.9,
          preview = { layout = 'horizontal' },
        },

        fn_transform = function(path)
          path = path:gsub('^󰉋' .. fzf.utils.nbsp, '')
          path = path:gsub('/%.[^/]*/$', '')
          return string.format('%-32s%s%s',
            fzf.utils.ansi_codes.cyan('󰂿 ' .. vim.fn.fnamemodify(path, ':t')),
            fzf.utils.nbsp,
            fzf.utils.ansi_codes.blue('󰉋' .. fzf.utils.nbsp .. vim.fn.fnamemodify(path, ':~'))
          )
        end,

        actions = {
          ['ctrl-g'] = actions.toggle_hidden,
          ['default'] = function(selected)
            if util.buffer_count() > 0 then
              vim.cmd.tabnew()
            end

            local file = fzf.path.entry_to_file(selected[1])
            vim.cmd('silent lcd ' .. file.path)
            fzf.files({ cwd = file.path })
          end
        }
      })

      fzf.files(settings)
    end

    map_fzf('<Leader>gp', projects, { desc = 'projects' })
  end
}
