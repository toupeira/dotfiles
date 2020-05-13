-- wait for window to be on correct monitor
require('socket').sleep(0.05)

-- get current window information
id    = get_window_xid()
title = get_window_name()
class = get_window_class()
type  = get_window_type()
role  = get_window_role()

-- get current monitor resolution
pipe = io.popen('xmonitorinfo ' .. id, 'r')
output = pipe:read('*a')
pipe:close()
parts = string.gmatch(output, '%d+')

MONITOR_WIDTH = tonumber(parts())
MONITOR_HEIGHT = tonumber(parts())
MONITOR_XOFFSET = tonumber(parts())
MONITOR_YOFFSET = tonumber(parts())

debug_print('--')
debug_print('Monitor:   ' .. MONITOR_WIDTH .. 'x' .. MONITOR_HEIGHT .. '+' .. MONITOR_XOFFSET .. '+' .. MONITOR_YOFFSET)
debug_print('ID:        ' .. id)
debug_print('Title:     ' .. title)
debug_print('Class:     ' .. class)
debug_print('Type:      ' .. type)

if type ~= "WINDOW_TYPE_NORMAL"
then
  debug_print('Ignoring this window...')
  return
end

-- windows on the left
-- if class == 'Nautilus'
-- or class == 'Evince'
-- or class == 'Gitg'
-- or class == 'Zeal'
-- or class == 'Electron'
-- then
--   debug_print('Moving to the left...')
--   set_window_position(MONITOR_XOFFSET, 0)
--   set_window_size(MONITOR_WIDTH / 2, MONITOR_HEIGHT)
--   return
-- end

-- windows on the right
if class == 'X-terminal-emulator'
or (class == 'Tilix' and role ~= 'quake')
then
  debug_print('Moving to the right...')
  set_window_position(MONITOR_XOFFSET + MONITOR_WIDTH / 2, 0)
  set_window_size(MONITOR_WIDTH / 2, MONITOR_HEIGHT)
  maximize_vertically()
  return
end

