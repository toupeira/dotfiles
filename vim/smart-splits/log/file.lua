-- disable file logger for smart-splits.nvim

local M = {
  append = function() end,
  read = function() end,
}

function M.init()
  return M
end

return M
