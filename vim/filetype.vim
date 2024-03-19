autocmd vimrc BufNewFile,BufRead Dangerfile,*.axlsx,*.prawn setlocal filetype=ruby
autocmd vimrc BufNewFile,BufRead *.bats setlocal filetype=bash
autocmd vimrc BufNewFile,BufRead *.es6,*.pac setlocal filetype=javascript
autocmd vimrc BufNewFile,BufRead *.run setlocal filetype=sh

" org-mode: silence warning about missing plugin
command! -nargs=* -range SpeedDatingFormat echo
