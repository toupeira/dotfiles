return {
  { 'elixir-lang/vim-elixir', ft = { 'elixir', 'eelixir' } },
  { 'habamax/vim-godot', ft = { 'gdscript', 'gsl' } },
  { 'hail2u/vim-css3-syntax', ft = { 'css', 'scss' } },
  { 'hashivim/vim-terraform', ft = { 'terraform', 'hcl' } },

  { 'iamcco/markdown-preview.nvim',
    ft = { 'markdown' },
    cmd = {
      'MarkdownPreviewToggle',
      'MarkdownPreview',
      'MarkdownPreviewStop',
    },
    build = function() vim.fn['mkdp#util#install']() end,
  },

  { 'vim-ruby/vim-ruby',
    ft = 'ruby',
    dependencies = { 'tpope/vim-rails' },

    init = function()
      vim.g.ruby_operators = 1
      vim.g.ruby_pseudo_operators = 1
      vim.g.ruby_no_expensive = 1

      vim.g.ruby_indent_block_style = 'do'
      vim.g.ruby_indent_assignment_style = 'variable'
      vim.g.ruby_indent_hanging_elements = 0
    end
  },
}
