local Color = require("nightfox.lib.color")
local theme = require('nightfox.util.lualine')('nordfox')

for _, groups in pairs(theme) do
  -- use mode colors as foreground for a and b sections
  -- TODO: add upstream
  groups.a.fg = groups.b.bg
  groups.b.fg = Color.from_hex(groups.a.bg):lighten(10):to_css()

  -- use mode color as foreground for x section
  groups.x = { fg = groups.a.bg, bg = theme.normal.c.bg }
end

return theme
