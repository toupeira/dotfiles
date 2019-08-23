if exists('g:GuiLoaded')
  GuiTabline 0
  GuiPopupmenu 0

  if hostname() == 'meerkat'
    GuiFont! Hack\ Nerd\ Font:h10
  elseif hostname() == 'schtube'
    GuiFont! Hack\ Nerd\ Font:h12
  else
    GuiFont! Hack\ Nerd\ Font:h9
  endif

  if &diff
    call GuiWindowMaximized(1)
    sleep 250m
    wincmd =
  endif
endif
