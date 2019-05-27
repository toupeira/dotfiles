if exists('g:GuiLoaded')
  GuiTabline 0
  GuiPopupmenu 0

  if hostname() == 'meerkat'
    GuiFont! DejaVuSansMono\ Nerd\ Font:h10
  elseif hostname() == 'schtube'
    GuiFont! DejaVuSansMono\ Nerd\ Font:h12
  else
    GuiFont! DejaVuSansMono\ Nerd\ Font:h9
  endif

  if &diff
    call GuiWindowMaximized(1)
    sleep 250m
    wincmd =
  endif
endif
