return {
  { 'antonk52/markdowny.nvim', ft = { 'markdown' }, config = true },
  { 'elixir-lang/vim-elixir',  ft = { 'elixir', 'eelixir' }},
  { 'habamax/vim-godot',       ft = { 'gdscript', 'gsl' }},
  { 'hail2u/vim-css3-syntax',  ft = { 'css', 'scss' }},
  { 'hashivim/vim-terraform',  ft = { 'terraform', 'hcl' }},

  { 'brianhuster/live-preview.nvim', -- {{{
    ft = { 'markdown', 'html', 'asciidoc', 'svg' },
    cmd = 'LivePreview',
    keys = function(plugin)
      return {
        {
          '<LocalLeader>P', function()
            if require('livepreview').is_running() then
              vim.cmd.LivePreview('close')
            else
              vim.cmd.LivePreview('start')
            end
          end,
          ft = plugin.ft,
          desc = 'Toggle live preview',
        },
      }
    end,
  },
  -- }}}
  { 'catgoose/nvim-colorizer.lua', -- {{{
    ft = { 'css', 'scss', 'lua' },
    cmd = 'ColorizerToggle',
    keys = {
      { '<LocalLeader>c', '<Cmd>ColorizerToggle<CR>', desc = 'Toggle colorizer' },
    },
    opts = function(plugin)
      return {
        lazy_load = true,
        filetypes = plugin.ft,
        user_default_options = {
          names = false,
        },
      }
    end,
  },
  -- }}}
  { 'OXY2DEV/helpview.nvim', -- {{{
    ft = { 'help' },
    cmd = 'Helpview',
  },
  -- }}}
  { 'OXY2DEV/markview.nvim', -- {{{
    ft = { 'markdown', 'codecompanion' },
    cmd = { 'Markview' },
    opts = function(plugin)
      local function conceal_tag(icon, hl_group)
        return {
          on_node = { hl_group = hl_group },
          on_closing_tag = { conceal = '' },
          on_opening_tag = {
            conceal = '',
            virt_text_pos = 'inline',
            virt_text = {{ icon .. ' ', hl_group }},
          },
        }
      end

      return {
        preview = {
          debounce = 10,
          filetypes = plugin.ft,
          ignore_buftypes = {},
          modes = { 'n', 'no', 'c', 'i' },
          hybrid_modes = { 'i' },
          linewise_hybrid_mode = true,
        },

        markdown = {
          list_items = {
            shift_width = 2,
            marker_minus       = { add_padding = false, text = '-' },
            marker_plus        = { add_padding = false, text = '●' },
            marker_star        = { add_padding = false, text = '●' },
            marker_dot         = { add_padding = false, conceal_on_checkboxes = false },
            marker_parenthesis = { add_padding = false, conceal_on_checkboxes = false },
          },
        },

        markdown_inline = {
          checkboxes = {
            checked = { text = '󰄲', hl = 'Keyword', scope_hl = 'MarkviewCheckboxStriked' },
            unchecked = { text = '󰄱', hl = 'Keyword', scope_hl = '' },
          },
        },

        html = {
          container_elements = {
            ['^buf$']         = conceal_tag('', 'CodeCompanionChatVariable'),
            ['^file$']        = conceal_tag('', 'CodeCompanionChatVariable'),
            ['^help$']        = conceal_tag('󰘥', 'CodeCompanionChatVariable'),
            ['^image$']       = conceal_tag('', 'CodeCompanionChatVariable'),
            ['^symbols$']     = conceal_tag('', 'CodeCompanionChatVariable'),
            ['^url$']         = conceal_tag('󰖟', 'CodeCompanionChatVariable'),
            ['^var$']         = conceal_tag('', 'CodeCompanionChatVariable'),
            ['^tool$']        = conceal_tag('', 'CodeCompanionChatTool'),
            ['^user_prompt$'] = conceal_tag('', 'CodeCompanionChatTool'),
            ['^group$']       = conceal_tag('', 'CodeCompanionChatToolGroup'),
          },
        },
      }
    end,
  },
  -- }}}
  { 'stevearc/quicker.nvim', -- {{{
    ft = 'qf',

    opts = {
      follow = { enabled = true },

      keys = {
        { '>', '<Cmd>lua require("quicker").expand({ before = 3, after = 3, add_to_existing = true })<CR>', desc = 'Expand quickfix context' },
        { '<', '<Cmd>lua require("quicker").collapse()<CR>', desc = 'Reduce quickfix context' },
      },

      max_filename_width = function()
        return math.floor(math.min(95, vim.o.columns / 3))
      end,
    },
  },
  -- }}}
  { 'vim-ruby/vim-ruby', -- {{{
    ft = 'ruby',
    init = function()
      vim.g.ruby_operators = 1
      vim.g.ruby_pseudo_operators = 1
      vim.g.ruby_no_expensive = 1

      vim.g.ruby_indent_block_style = 'do'
      vim.g.ruby_indent_assignment_style = 'variable'
      vim.g.ruby_indent_hanging_elements = 0
    end,
  },
  -- }}}
}
