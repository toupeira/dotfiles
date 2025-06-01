local util = require('util')

return {
  'olimorris/codecompanion.nvim',
  dependencies = {
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
            return string.format(
              'Response from %s (%s) 📌',
              adapter.formatted_name,
              adapter.model.name
                :gsub('.*/', '')
                :gsub('claude%-opus%-4[^:]*',     'opus-4')
                :gsub('claude%-sonnet%-4[^:]*',   'sonnet-4')
                :gsub('claude%-3.7%-sonnet[^:]*', 'sonnet-3.7')
                :gsub('claude%-3.5%-sonnet[^:]*', 'sonnet-3.5')
                :gsub('claude%-3.5%-haiku[^:]*',  'haiku-3.7')
                :gsub('deepseek.*v3[^:]*',        'deepseek-3')
                :gsub('deepseek.*r1[^:]*',        'deepseek-r1')
                :gsub('gemini.*pro[^:]*',         'gemini-pro')
                :gsub('gemini.*flash[^:]*',       'gemini-flash')
            )
          end,
          user = 'Me ⚗️',
        },

        tools = {
          opts = {
            auto_submit_errors = true,
            auto_submit_success = false,
          },

          plan = {
            callback = 'strategies.chat.agents.tools.plan',
            description = 'Manage an internal todo list',
          },
        },

        variables = {
          buffer = {
            opts = { default_params = 'watch' },
          },
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

    prompt_library = {
      ['Edit'] = {
        strategy = 'chat',
        description = 'Edit the current buffer',
        prompts = {
          { role = 'user', content = '@editor #buffer\n\n' },
        },
        opts = {
          auto_submit = false,
          short_name = 'edit',
          is_slash_cmd = true,
        },
      },
      ['Develop'] = {
        strategy = 'chat',
        description = 'Edit with full tooling',
        prompts = {
          { role = 'user', content = '@full_stack_dev #buffer\n\n' },
        },
        opts = {
          auto_submit = false,
          short_name = 'dev',
          is_slash_cmd = true,
        },
      },
    },

    display = {
      action_palette = { provider = 'default' },

      chat = {
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
  end,

  init = function()
    util.command('AI', function(opts)
      local args = #opts.args > 0 and opts.args or nil
      local codecompanion = require('codecompanion')
      local config = require('codecompanion.config')

      if opts.bang then
        -- called with a bang: generate a Vim command
        vim.cmd.CodeCompanionCmd({ args = { args }})
      elseif opts.range ~= 0 then
        -- called with a selection: open the inline assistant
        vim.cmd.echo()
        vim.cmd.CodeCompanion({
          args = { args },
          range = { opts.line1, opts.line2 },
        })
      else
        -- called normally: toggle the chat
        vim.cmd.echo()
        if codecompanion.last_chat() or args then
          vim.cmd.CodeCompanionChat({ args = { args or 'Toggle' }})
        else
          config.display.chat.start_in_insert_mode = true
          vim.cmd.CodeCompanionChat()
          config.display.chat.start_in_insert_mode = false
        end
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
