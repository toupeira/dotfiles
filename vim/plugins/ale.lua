local util = require('util')

return {
  'dense-analysis/ale',
  event = 'LazyFile',
  dependencies = { 'mason.nvim' },

  init = function()
    vim.g.ale_use_neovim_diagnostics_api = 1
    vim.g.ale_disable_lsp = 1

    vim.g.ale_set_loclist = 0
    vim.g.ale_set_quickfix = 0
    vim.g.ale_echo_cursor = 0
    vim.g.ale_virtualtext_cursor = 'disabled'

    vim.g.ale_lint_on_enter = 1
    vim.g.ale_lint_on_save = 1
    vim.g.ale_lint_on_insert_leave = 1
    vim.g.ale_lint_on_text_changed = 'normal'

    vim.g.ale_fix_on_save = 1
    vim.g.ale_fix_on_save_ignore = { 'rubocop' }

    vim.g.ale_linters = {
      javascript = { 'eslint' },
      json = { 'jsonlint' },
      ruby = { 'ruby', 'rubocop' },
      typescript = { 'eslint' },
      yaml = { 'yamllint' },
    }

    vim.g.ale_fixers = {
      javascript = { 'prettier' },
      -- lua = { 'stylua' },
      ruby = { 'rubocop' },
      vue = { 'prettier' },
    }

    vim.g.ale_ruby_rubocop_auto_correct_all = 1
    vim.g.ale_lua_luacheck_options = '--globals vim --max-line-length 160'
    vim.g.ale_lua_stylua_options = '--verify'
    vim.g.ale_terraform_terraform_executable = 'tofu'

    util.autocmd({ 'BufRead', 'BufNewFile' }, '.env', function()
      vim.b.ale_sh_shellcheck_exclusions = 'SC2034'
    end)
  end,
}
