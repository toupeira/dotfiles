if vim.g.neovide then
  local font, _ = vim.fn.system(
    'dconf read /org/gnome/desktop/interface/monospace-font-name'
  ):gsub('\n*', ''):gsub("'", ''):gsub(' ([0-9])', ':h%1')

  vim.o.guifont = font
end
