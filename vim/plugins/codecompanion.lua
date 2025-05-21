local util = require('util')

return {
  'olimorris/codecompanion.nvim',
  dependencies = {
    'fidget.nvim',
    'nvim-treesitter',
    'nvim-lua/plenary.nvim',
  },

  cmd = {
    'CodeCompanion',
    'CodeCompanionActions',
    'CodeCompanionChat',
    'CodeCompanionCmd',
  },

  keys = {
    { '<Leader>ai', ':AI<CR>', mode = { 'n', 'v' }, desc = 'AI: Toggle chat' },
    { '<Leader>aa', '<Cmd>CodeCompanionActions<CR>', mode = { 'n', 'v' }, desc = 'AI: Show actions' },
  },

  opts = {
    adapters = {
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
        vim.cmd.CodeCompanionCmd({ args = { args }})
      elseif opts.range == 0 then
        vim.cmd.echo()
        vim.cmd.CodeCompanionChat({ args = { args or 'Toggle' }})
      else
        vim.cmd.echo()
        vim.cmd.CodeCompanion({
          args = { args },
          range = { opts.line1, opts.line2 },
        })
      end
    end, { nargs = '*', range = true, bang = true }, 'AI: Toggle chat')

    util.alias_command({
      ai = 'AI', aI = 'AI', Ai = 'AI',
    })

    util.nmap('<Leader>aA', function()
      require('codecompanion.strategies.inline'):stop()
    end, 'AI: Cancel request')

    util.autocmd('FileType', 'codecompanion', function ()
      util.nmap('q', '<Cmd>CodeCompanionChat Toggle<CR>', 'AI: Hide chat', { buffer = true, force = true })
    end)
  end
}
