return {
  'mhinz/vim-startify',

  config = function()
    vim.cmd([[
      function! StartifyEntryFormat() abort
        return "v:lua.require('nvim-web-devicons').get_icon(absolute_path, fnamemodify(absolute_path, ':e'), { 'default': 1 }) . ' ' . entry_path"
      endfunction

      let g:startify_change_to_dir = 0
      let g:startify_update_oldfiles = 1
      let g:startify_files_number = 9
      let g:startify_custom_indices = map(range(1,9), 'string(v:val)')

      let g:startify_lists = []

      let s:cwd = fnamemodify(getcwd(), ':~')
      if s:cwd == '~'
        let g:startify_lists += [{ 'type': 'files', 'header': ['   Recent' ] }]
      else
        let g:startify_lists += [{ 'type': 'dir', 'header': ['   Recent in ' . s:cwd] }]
        f
      endif

      let g:startify_lists += [{ 'type': 'bookmarks', 'header': ['   Bookmarks'] }]

      let g:startify_bookmarks = [
        \ {'v': '~/.config/nvim/init.lua'},
        \ {'g': '~/.config/git/config'},
        \ {'t': '~/.config/tmux/tmux.conf'},
      \ ]

      let g:startify_ascii = [
        \ "      .            .",
        \ "    .,;'           :,.",
        \ "  .,;;;,,.         ccc;.",
        \ ".;c::::,,,'        ccccc:",
        \ ".::cc::,,,,,.      cccccc.",
        \ ".cccccc;;;;;;'     llllll.",
        \ ".cccccc.,;;;;;;.   llllll.",
        \ ".cccccc  ';;;;;;'  oooooo.",
        \ "'lllllc   .;;;;;;;.oooooo'",
        \ "'lllllc     ,::::::looooo'",
        \ "'llllll      .:::::lloddd'",
        \ ".looool       .;::coooodo.",
        \ "  .cool         'ccoooc.",
        \ "    .co          .:o:.",
        \ "      .           .'",
      \ ]

      let g:startify_custom_header = map(g:startify_ascii, '"   ".v:val')
    ]])
  end
}
