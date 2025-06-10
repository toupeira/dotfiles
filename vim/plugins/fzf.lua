local util = require('util')
local merge = util.merge

local expand = vim.fn.expand

local presets = {
  -- fzf-lua options

  preview = {
    winopts = { preview = { hidden = 'nohidden' }},
  },

  reverse = {
    fzf_opts = { ['--layout'] = 'reverse-list', ['--no-sort'] = true },
  },

  bottom = {
    -- from ivy profile
    winopts = {
      row = 1,
      col = 0,
      width = 1,
      height = 0.4,
      title_pos = 'left',
      border = { '╭', '─', '┬', '│', '┴', '─', '╰', '│' },
      preview = {
        layout = 'horizontal',
        title_pos = 'left',
        border = { '┬', '─', '╮', '│', '╯', '─', '─', '' },
      },
    },
  },

  -- fzf-lua arguments

  title = function(value)
    return { winopts = { title = ' ' .. value .. ' ' }}
  end,

  cwd = function()
    return { cwd = expand('%:h') }
  end,

  cword = function()
    return vim.bo.filetype == 'ministarter' and {} or { query = expand('<cword>') }
  end,

  hidden = function()
    return util.is_home and { hidden = false } or {}
  end,
}

return {
  'ibhagwan/fzf-lua',
  event = 'VeryLazy',

  opts = function()
    local fzf = require('fzf-lua')

    return {
      winopts = {
        width = 0.9,
        height = 0.9,
        row = 0.5,

        preview = {
          hidden = 'hidden',
          vertical = 'up:60%',
          horizontal = 'right:50%',
        },

        -- restore defaults from fzf.sh
        treesitter = {
          fzf_colors = {
            ['hl'] = '#d7ffaf:bold',
            ['hl+'] = '#ecffd9',
          },
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
        -- restore defaults from fzf.sh
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

      -- add directory previewer
      previewers = {
        tree = {
          cmd = 'tree',
          args = '-dxC --gitignore --prune --noreport',
          _ctor = require('fzf-lua.previewer').fzf.cmd,
        },
      },

      -- provider settings

      defaults = {
        color_icons = true,
      },

      blines = presets.reverse,
      builtin = presets.reverse,
      helptags = presets.preview,
      highlights = presets.preview,
      jumps = presets.preview,
      lines = presets.reverse,
      manpages = presets.preview,
      marks = presets.preview,
      treesitter = merge(presets.bottom, presets.preview),

      buffers = merge(presets.bottom, presets.preview, {
        fzf_opts = { ['--header-lines'] = false },
        formatter = "path.filename_first",
      }),

      colorschemes = merge(presets.reverse, {
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
      }),

      diagnostics = merge(presets.bottom, presets.preview, {
        color_icons = true,
        color_headings = true,
        diag_source = true,
        diag_code = true,
        sort = 1,
      }),

      files = {
        fd_opts = '--color always --max-results 99999 --type f --type l --exclude .git',
        hidden = true,
        git_icons = false,
        no_header = true,

        actions = {
          ['ctrl-g'] = fzf.actions.toggle_ignore,
          ['alt-i'] = false,
          ['alt-h'] = false,
          ['alt-f'] = false,
        },
      },

      git = {
        branches = merge(presets.bottom, presets.preview),
        bcommits = merge(presets.preview, presets.reverse, presets.title('Git Log (Buffer)')),
        commits = merge(presets.preview, presets.reverse, presets.title('Git Log (Project)')),
        diff = merge(presets.bottom, presets.preview),
        status = merge(presets.bottom, presets.preview),
      },

      grep = {
        multiline = 1,
        RIPGREP_CONFIG_PATH = vim.env.RIPGREP_CONFIG_PATH,
      },

      lsp = {
        finder = presets.preview,
        code_actions = presets.preview,
      },

      oldfiles = merge(presets.title('History'), {
        include_current_session = true,
        file_ignore_patterns = { '%.git/COMMIT_EDITMSG' },
      }),

      btags = merge(presets.bottom, presets.preview, presets.title('Tags (Buffer)'), {
        ctags_args = '-f- -u',
      }),

      tags = merge(presets.bottom, presets.preview, presets.title('Tags (Project)'), {
        ctags_autogen = true,
        cmd = '( git ls-files 2>/dev/null || fdfind ) | ctags -f- -L- -u',
      }),
    }
  end,

  config = function(_, opts)
    local fzf = require('fzf-lua')

    -- use history per provider
    vim.g.fzf_history_dir = vim.fn.stdpath('state') .. '/fzf'

    fzf.setup(opts)

    fzf.register_ui_select(function(select_opts, items)
      local title = select_opts.prompt
      select_opts.prompt = '» '

      local height = math.min(
        presets.bottom.winopts.height,
        (#items + 2) / vim.o.lines
      )

      return merge(presets.bottom, presets.reverse, presets.title(title), {
        winopts = {
          border = 'rounded',
          height = height,
        },
      })
    end)

    util.alias_command({
      fzf = 'FzfLua', Fzf = 'FzfLua', FZF = 'FzfLua', FZf = 'FzfLua',
    })

    -- when opening multiple files, open both the quickfix list and the first file
    fzf.defaults.actions.files.default = function(selected, settings)
      fzf.actions.file_edit({ selected[1] }, settings)

      if #selected > 1 then
        fzf.actions.file_sel_to_qf(selected, settings)
        vim.cmd.wincmd('p')
      end
    end

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
    map_fzf('<Leader>f', 'files', { args = presets.hidden })
    map_fzf('<Leader>F', 'files', {
      desc = 'files in current directory',
      args = function()
        return merge(
          presets.title('Files (current directory)'),
          presets.cwd()
        )
      end,
    })
    map_fzf('<Leader>o', 'files', {
      desc = 'Obsidian notes',
      args = merge(presets.title('Notes'), { cwd = '/slack/documents/Notes', fd_opts = '-e md' }),
    })

    map_fzf('<Leader>b', 'buffers')
    map_fzf('<Leader>B', 'buffers', {
      desc = 'all buffers',
      args = merge(presets.title('Buffers (All)'), { show_unlisted = true }),
    })

    map_fzf('<Leader>h', 'oldfiles', { desc = 'history' })
    map_fzf('<Leader>H', 'oldfiles', {
      desc = 'history in project',
      args = merge(presets.title('History (Project)'), { cwd_only = true }),
    })

    -- search file contents
    map_fzf('<Leader>r', 'live_grep', {
      desc = 'regex in project',
      args = presets.cword,
    })
    map_fzf('<Leader>R', 'live_grep', {
      desc = 'regex in current directory',
      args = function()
        return merge(
          presets.title('Grep (current directory)'),
          presets.cwd(),
          presets.cword()
        )
      end,
    })

    map_fzf('<Leader>l', 'blines', { desc = 'lines in buffer' })
    map_fzf('<Leader>t', 'btags', { desc = 'buffer symbols' })
    map_fzf('<Leader>T', 'tags', { desc = 'project symbols' })

    map_fzf('<Leader>d', 'diagnostics_document', { desc = 'diagnostics in current file' })
    map_fzf('<Leader>D', 'diagnostics_workspace', {
      desc = 'diagnostics in project',
      args = presets.title('Diagnostics (Workspace)'),
    })

    -- search vim history
    map_fzf('<Leader>:', 'command_history', { mode = { 'n', 'v' }})
    map_fzf('<Leader>/', 'search_history')

    -- search vim internals
    map_fzf('<F1><F1>', 'help_tags')
    map_fzf('<F1>C', 'colorschemes')
    map_fzf('<F1>c', 'commands')
    map_fzf('<F1>h', 'highlights')
    map_fzf('<F1>j', 'jumps')
    map_fzf('<F1>k', 'keymaps')
    map_fzf('<F1>M', 'man_pages')
    map_fzf('<F1>m', 'marks')
    map_fzf('<F1>r', 'registers')

    map_fzf('<F1>f', 'builtin', { desc = 'FZF providers' })

    -- search spellcheck
    map_fzf('<Leader>z', 'spell_suggest', { desc = 'spelling suggestions' })

    -- search LSP
    map_fzf('<Leader>La', 'lsp_code_actions')
    map_fzf('<Leader>Ld', 'lsp_definitions')
    -- map_fzf('<Leader>LD', 'lsp_declarations')
    -- map_fzf('<Leader>Li', 'lsp_implementations')
    map_fzf('<Leader>LI', 'lsp_incoming_calls')
    map_fzf('<Leader>LO', 'lsp_outgoing_calls')
    map_fzf('<Leader>Lr', 'lsp_references')
    map_fzf('<Leader>Lt', 'lsp_document_symbols')
    map_fzf('<Leader>LT', 'lsp_live_workspace_symbols')
    -- map_fzf('<Leader>Lt', 'lsp_typedefs')

    -- search git
    map_fzf('<Leader>gm', 'git_status')
    map_fzf('<Leader>gc', 'git_branches')
    map_fzf('<Leader>gl', 'git_bcommits')
    map_fzf('<Leader>gL', 'git_commits')

    -- search Aerial symbols
    map_fzf('<Leader>i', function()
      require('aerial').fzf_lua_picker(merge(presets.bottom, presets.preview))
    end, { desc = 'symbols' })

    -- search projects
    local projects = function(settings)
      settings = merge(presets.bottom, presets.title('Projects'), {
        fd_opts = '-u --glob --type d ".{git,obsidian}" --exclude .stversions ~/src /slack',
        toggle_hidden_flag = '--exclude "{archive,packages}"',
        fzf_opts = { ['--multi'] = false },
        previewer = 'tree',
        cwd_prompt = false,

        fn_transform = function(path)
          path = path:gsub('/%.[^/]*/$', '')
          return string.format('%-32s%s%s',
            fzf.utils.ansi_codes.cyan('󰂿 ' .. vim.fn.fnamemodify(path, ':t')),
            fzf.utils.nbsp,
            fzf.utils.ansi_codes.blue(vim.fn.fnamemodify(path, ':~'))
          )
        end,

        actions = {
          ['ctrl-g'] = fzf.actions.toggle_hidden,
          ['default'] = function(selected)
            if util.buffer_count() > 0 then
              vim.cmd.tabnew()
            end

            local file = fzf.path.entry_to_file(selected[1])
            vim.cmd('silent lcd ' .. file.path)
            fzf.files({ cwd = file.path })
          end
        }
      }, settings or {})

      fzf.files(settings)
    end

    map_fzf('<Leader>gp', projects, { desc = 'projects' })
  end,
}
