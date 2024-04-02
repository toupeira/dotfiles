return {
  { 'elixir-lang/vim-elixir' },
  { 'habamax/vim-godot' },
  { 'hail2u/vim-css3-syntax' },

  { 'preservim/vim-markdown',
    config = function()
      vim.g.vim_markdown_folding_disabled = 0
      vim.g.vim_markdown_folding_style_pythonic = 1
      vim.g.vim_markdown_frontmatter = 1
      vim.g.vim_markdown_new_list_item_indent = 2
      vim.g.vim_markdown_strikethrough = 1

      -- https://github.com/tpope/vim-markdown
      vim.g.markdown_recommended_style = 0
    end
  },

  { 'vim-ruby/vim-ruby',
    config = function()
      vim.g.ruby_operators = 1
      vim.g.ruby_pseudo_operators = 1
      vim.g.ruby_no_expensive = 1

      vim.g.ruby_indent_block_style = 'do'
      vim.g.ruby_indent_assignment_style = 'variable'
      vim.g.ruby_indent_hanging_elements = 0

      vim.cmd('hi! link rubyRailsARMethod Statement')
    end
  },
}
