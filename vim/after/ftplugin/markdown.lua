vim.b.minihipatterns_config = {
  highlighters = {
    tags = {
      pattern = '%s#%a+[%a%d_/-]*[%a%d]+',
      group = '@markup.math',
    },

    tags_start_of_line = {
      pattern = '^#%a+[%a%d_/-]*[%a%d]+',
      group = '@markup.math',
    },
  },
}
