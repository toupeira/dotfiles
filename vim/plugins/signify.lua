return {
  'mhinz/vim-signify',
  event = 'VeryLazy',

  config = function()
    vim.g.signify_vcs_list = { 'git' }

    vim.g.signify_sign_add               = '+'
    vim.g.signify_sign_delete            = '-'
    vim.g.signify_sign_delete_first_line = '^'
    vim.g.signify_sign_change            = '•'
  end
}
