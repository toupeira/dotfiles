-- wait for window to be on correct monitor
require('socket').sleep(0.05)

function debug_print(message)
  if DEBUG then
    print(message)
  end
end

-- get current window information
id    = window_xid()
title = window_title()
class = window_class()
type  = window_type()

-- get current monitor resolution
pipe = io.popen('xmonitorinfo ' .. id, 'r')
output = pipe:read('*a')
pipe:close()
parts = string.gmatch(output, '%d+')

MONITOR_WIDTH = parts()
MONITOR_HEIGHT = parts()
MONITOR_XOFFSET = parts()
MONITOR_YOFFSET = parts()

if DEBUG then
  debug_print('--')
  debug_print('Monitor:   ' .. MONITOR_WIDTH .. 'x' .. MONITOR_HEIGHT .. '+' .. MONITOR_XOFFSET .. '+' .. MONITOR_YOFFSET)
  debug_print('ID:        ' .. id)
  debug_print('Workspace: ' .. workspace())
  debug_print('Title:     ' .. title)
  debug_print('Class:     ' .. class)
  debug_print('Type:      ' .. type)
end

if type ~= "WINDOW_NORMAL"
then
  debug_print('Ignoring this window...')
  return
end

-- windows on the left
if class == 'Nautilus'
or class == 'Quodlibet'
or class == 'Evince'
or class == 'Gitg'
or class == 'Zeal'
or class == 'Electron'
or (class == 'X-terminal-emulator' and title == 'Terminator Preferences')
then
  debug_print('Moving to the left...')
  xy(MONITOR_XOFFSET, 0)
  size(MONITOR_WIDTH / 2, MONITOR_HEIGHT)
  maximize_vertically()
  return
end

-- windows on the right
if class == 'X-terminal-emulator'
or class == 'Xfce4-terminal'
or class == 'Terminator'
or class == 'kitty'
or class == 'nvim-qt'
or class == 'Gvim'
or class == 'Emacs'
then
  debug_print('Moving to the right...')
  xy(MONITOR_XOFFSET + MONITOR_WIDTH / 2, 0)
  size(MONITOR_WIDTH / 2, MONITOR_HEIGHT)
  maximize_vertically()
  return
end
