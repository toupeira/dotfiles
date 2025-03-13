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
      ruby = { 'ruby', 'rubocop' },
      json = { 'jsonlint' },
      javascript = { 'eslint' },
      typescript = { 'eslint' },
    }

    vim.g.ale_fixers = {
      javascript = { 'prettier' },
      ruby = { 'rubocop' },
      vue = { 'prettier' },
    }

    vim.g.ale_ruby_rubocop_auto_correct_all = 1
    vim.g.ale_lua_luacheck_options = '--globals vim --max-line-length 150'
  end
}
