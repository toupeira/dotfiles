if exists('g:GuiLoaded')
  GuiTabline 0
  GuiPopupmenu 0

  if hostname() == 'meerkat'
    GuiFont! Hack\ Nerd\ Font:h10.5
  else
    GuiFont! Hack\ Nerd\ Font:h10
  endif

  if &diff
    call GuiWindowMaximized(1)
    sleep 250m
    wincmd =
  endif
endif
