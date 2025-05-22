local util = require('util')

return {
  'olimorris/codecompanion.nvim',
  dependencies = {
    'fidget.nvim',
    'nvim-treesitter',
    'nvim-lua/plenary.nvim',

    'ravitemer/codecompanion-history.nvim',

    {
      'ravitemer/mcphub.nvim',
      build = 'bundled_build.lua',
      cmd = 'MCPHub',
      keys = {
        { '<Leader>$', '<Cmd>MCPHub<CR>', desc = 'Open MCB Hub' },
      },

      opts = {
        shutdown_delay = 0,
        use_bundled_binary = true,
        auto_toggle_mcp_servers = false,
      },
    },
  },

  cmd = {
    'CodeCompanion',
    'CodeCompanionActions',
    'CodeCompanionChat',
    'CodeCompanionCmd',
    'CodeCompanionHistory',
  },

  keys = {
    { '<Leader>aa', ':AI<CR>', mode = { 'n', 'v' }, desc = 'AI: Toggle chat' },
    { '<Leader>aA', '<Cmd>CodeCompanionActions<CR>', mode = { 'n', 'v' }, desc = 'AI: Show actions' },
    { '<Leader>aS', 'lua require("codecompanion.strategies.inline"):stop()', desc = 'AI: Stop inline request' },
    { '<Leader>ah', '<Cmd>CodeCompanionHistory<CR>', desc = 'AI: Show chat history' },
  },

  opts = {
    adapters = {
      opts = {
        show_defaults = false,
        show_model_choices = true,
      },

      anthropic = function()
        return require('codecompanion.adapters').extend('anthropic', {
          schema = {
            model = {
              default = 'claude-3-5-haiku-latest',
              -- default = 'claude-3-7-sonnet-latest',
            },
          },
        })
      end,

      gemini = function()
        return require('codecompanion.adapters').extend('gemini', {
          schema = {
            model = {
              default = 'gemini-2.5-flash-preview-04-17',
            },
          },
        })
      end,

      openrouter = function()
        return require('codecompanion.adapters').extend('openai_compatible', {
          formatted_name = 'OpenRouter',
          env = {
            url = 'https://openrouter.ai/api',
            api_key = 'OPENROUTER_API_KEY',
            chat_url = '/v1/chat/completions',
          },
          schema = {
            model = {
              default = 'google/gemini-2.5-pro-preview',
              -- default = 'google/gemini-2.5-flash-preview',
              -- default = 'anthropic/claude-3.7-sonnet',
            },
          },
        })
      end,
    },

    strategies = {
      cmd    = { adapter = 'anthropic' },
      inline = { adapter = 'anthropic' },
      chat   = { adapter = 'anthropic',
        keymaps = {
          close = { modes = { n = 'Q', i = '<Nop>' }},
          send =  { modes = { n = '<C-s>' }},
          stop =  { modes = { n = 'gA' }},
        },
      },
    },

    display = {
      action_palette = { provider = 'default' },

      chat = {
        window = {
          layout = 'horizontal',
          width = 0.4,
          height = 0.3,

          opts = {
            numberwidth = vim.o.numberwidth,
            signcolumn = vim.o.signcolumn,
            relativenumber = false,
            winfixheight = true,
          },
        },
      },
    },

    extensions = {
      history = {
        enabled = true,
        opts = {
          auto_save = true,
          save_chat_keymap = 'gS',
          picker = 'fzf-lua',
        },
      },

      mcphub = {
        callback = 'mcphub.extensions.codecompanion',
        opts = {
          show_result_in_chat = true,
          make_vars = true,
          make_slash_commands = true,
        },
      },
    },
  },

  config = function(_, opts)
    require('codecompanion').setup(opts)

    local slash_commands = require('codecompanion.config').strategies.chat.slash_commands
    for _, cmd in pairs(slash_commands) do
      if cmd.opts.provider == 'mini_pick' then
        cmd.opts.provider = 'fzf_lua'
      end
    end

    require('util.fidget-codecompanion'):init()
  end,

  init = function()
    util.command('AI', function(opts)
      local args = #opts.args > 0 and opts.args or nil

      if opts.bang then
        -- called with a bang: generate a Vim command
        vim.cmd.CodeCompanionCmd({ args = { args }})
      elseif opts.range == 0 then
        -- called normally: toggle the chat
        vim.cmd.echo()
        vim.cmd.CodeCompanionChat({ args = { args or 'Toggle' }})
      else
        -- called with a selection: open the inline assistant
        vim.cmd.echo()
        vim.cmd.CodeCompanion({
          args = { args },
          range = { opts.line1, opts.line2 },
        })
      end
    end, {
      desc = 'AI: Toggle chat',
      nargs = '*',
      range = true,
      bang = true,
      complete = function(arg_lead, cmdline, _)
        local commands = require('codecompanion.commands')
        return commands[1].opts.complete(arg_lead, cmdline:gsub('AI', 'CodeCompanion', 1))
      end
    })

    util.alias_command({
      ai = 'AI', aI = 'AI', Ai = 'AI',
    })

    -- hide the chat with 'q', close it with 'Q'
    util.autocmd('FileType', 'codecompanion', function (event)
      MiniClue.enable_buf_triggers(event.buf)
      util.nmap('q', '<Cmd>CodeCompanionChat Toggle<CR>', 'AI: Hide chat', { buffer = true, force = true })
    end)
  end
}
