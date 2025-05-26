local Color = require("nightfox.lib.color")
local theme = require('nightfox.util.lualine')('nordfox')

for _, groups in pairs(theme) do
  -- use mode colors as foreground for a/b/x sections
  groups.a.fg = Color.from_hex(groups.b.bg):lighten(0):to_css()
  groups.b.fg = Color.from_hex(groups.a.bg):lighten(10):to_css()
  groups.x = { fg = groups.a.bg }
end

return theme
