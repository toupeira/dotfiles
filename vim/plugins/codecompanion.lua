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

        ui = {
          window = { width = 0.9, height = 0.9 },
        },
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
    { '<M-a>', '<Cmd>AI<CR>', mode = { 'n', 'v' }, desc = 'AI: Toggle chat' },
    { '<Leader>an', '<Cmd>CodeCompanionChat<CR>', desc = 'AI: Start a new chat' },
    { '<Leader>aa', '<Cmd>CodeCompanionActions<CR>', mode = { 'n', 'v' }, desc = 'AI: Show actions' },
    { '<Leader>aS', '<Cmd>lua require("codecompanion.strategies.inline"):stop()<CR>', desc = 'AI: Stop inline request' },
    { '<Leader>ah', '<Cmd>CodeCompanionHistory<CR>', desc = 'AI: Show chat history' },
  },

  opts = {
    adapters = {
      opts = {
        show_defaults = false,
        show_model_choices = true,
      },

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

      claude = function()
        return require('codecompanion.adapters').extend('anthropic', {
          formatted_name = 'Claude',
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
    },

    strategies = {
      cmd    = { adapter = 'claude' },
      inline = { adapter = 'claude' },
      chat   = { adapter = 'claude',
        roles = {
          llm = function(adapter)
            return 'Response from ' .. adapter.formatted_name .. ' 📌'
          end,
          user = 'Me ⚗️',
        },

        variables = {
          buffer = { default_params = 'watch' },
        },

        keymaps = {
          close = { modes = { n = 'Q', i = '<Nop>' }},
          stop =  { modes = { n = '<Leader>aS' }},
          send =  {
            modes = { n = '<C-s>' },
            callback = function(chat)
              if vim.fn.mode() == 'i' then
                vim.cmd.stopinsert()
              end

              require('codecompanion.strategies.chat.keymaps').send.callback(chat)
            end
          },
        },
      },
    },

    display = {
      action_palette = { provider = 'default' },

      chat = {
        intro_message = 'Press ? for options',
        icons = { pinned_buffer = '📌 ' },

        window = {
          layout = 'horizontal',
          width = 0.4,
          height = 0.4,

          opts = {
            numberwidth = vim.o.numberwidth,
            signcolumn = vim.o.signcolumn,
            relativenumber = false,
          },
        },
      },
    },

    extensions = {
      history = {
        enabled = true,
        opts = {
          auto_save = true,
          expiration_days = 30,
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

    -- auto-watch buffers
    -- https://github.com/olimorris/codecompanion.nvim/pull/1516
    local buffer = require('codecompanion.strategies.chat.variables.buffer')
    local original_new = buffer.new
    buffer.new = function(args)
      args.params = args.params or 'watch'
      return original_new(args)
    end
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
  end,
}
