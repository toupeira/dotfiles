local fzf = require('lualine.extensions.fzf')

-- don't show current FZF selection in lualine
fzf.sections.lualine_y = nil

return fzf
