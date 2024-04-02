local fzf = require('fzf-lua')

local backends = require('aerial.backends')
local data = require('aerial.data')
local navigation = require('aerial.navigation')

-- TODO: add icon and source line
local get_symbols = function()
  local backend = backends.get()
  if not backend then
    backends.log_support_err()
    return
  elseif not data.has_symbols(0) then
    backend.fetch_symbols_sync(0)
  end

  local results = {}
  if data.has_symbols(0) then
    for _, item in data.get_or_create(0):iter({ skip_hidden = false }) do
      local label = string.format("%d: %s", item.idx, item.name)
      table.insert(results, label)
    end
  end

  return results
end

local goto_symbol = function(symbol)
  local idx = tonumber(symbol:match("^(%d+)"))
  -- FIXME this fails if the symbol is currently hidden in the tree
  for i, _, symbol_idx in data.get_or_create(0):iter({ skip_hidden = true }) do
    if idx == i then
      navigation.select({
        index = symbol_idx,
      })
      break
    end
  end
end

-- TODO: add preview
return function()
  local symbols = get_symbols()
  if not symbols then return end

  fzf.fzf_exec(symbols, {
    prompt = 'Symbols» ',
    fzf_opts = { ['--layout'] = 'reverse-list' },
    actions = {
      default = function(selected)
        goto_symbol(selected[1])
        vim.cmd.normal('zv')
      end
    }
  })
end
