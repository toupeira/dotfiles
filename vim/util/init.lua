local util = {}

local expand = vim.fn.expand
local fnamemodify = vim.fn.fnamemodify
local line = vim.fn.line
local pathshorten = vim.fn.pathshorten
local winnr = vim.fn.winnr

-- Lua helpers --------------------------------------------------------- {{{

util.split = vim.fn.split
util.join = table.concat

-- Merge the {opts} table into {defaults}.
util.merge = function(defaults, opts, ...)
  return vim.tbl_deep_extend('force', defaults or {}, opts or {}, ...)
end

-- Clamp a value to {min} and {max}
util.clamp = function(value, min, max)
  return math.max(min, math.min(max, value))
end

-- }}}
-- Configuration helpers ----------------------------------------------- {{{

util.is_home = vim.fn.getcwd() == os.getenv('HOME')
util.is_sudo = os.getenv('SUDO_COMMAND') ~= nil
util.is_ssh = os.getenv('SSH_CONNECTION') ~= nil
util.is_tmux = os.getenv('TMUX') ~= nil
util.is_headless = #vim.api.nvim_list_uis() == 0

-- Return active LSP clients for the current buffer
util.lsp_clients = function()
  return vim.lsp.get_clients({ bufnr = vim.api.nvim_get_current_buf() })
end

-- Load plugins after startup
util.very_lazy = function(plugin)
  plugin.event = 'VeryLazy'
  return plugin
end

util.lazy_file = function(plugin)
  plugin.event = 'LazyFile'
  return plugin
end

-- Map a key with sensible defaults.
util.map = function(mode, lhs, rhs, opts, desc)
  opts = opts or {}

  -- map multiple keys
  if type(lhs) == 'table' then
    for _, map in ipairs(lhs) do
      util.map(mode, map, rhs, opts, desc)
    end

    return
  end

  -- use string arguments as description
  if type(opts) == 'string' then opts = { desc = opts } end
  if type(desc) == 'string' then opts = util.merge(opts, { desc = desc }) end
  if type(desc) == 'table'  then opts = util.merge(opts, desc) end

  -- run commands silently
  if type(rhs) == 'string' then
    if rhs:sub(1, 1) == ':' then
      if mode ~= 'v' and not vim.deep_equal(mode, { 'n', 'v', 'o' }) then
        rhs = '<Cmd>' .. rhs:sub(2)
      end

      opts = util.merge(opts, { silent = true })
      rhs = rhs .. '<CR>'
    elseif rhs:sub(1, 5) == '<Cmd>' then
      opts = util.merge(opts, { silent = true })
      rhs = rhs .. '<CR>'
    end
  end

  -- run multiple commands
  if type(rhs) == 'table' then
    opts = util.merge(opts, { silent = true })
    local cmd = util.join(rhs, '\n')
    rhs = function () vim.cmd(cmd) end
  end

  -- check for existing maps
  local force = opts.force
  opts.force = nil

  if force ~= true then
    local modes = type(mode) == 'table' and mode or { mode }
    for _, m in ipairs(modes) do
      local mapping = vim.fn.maparg(lhs, m)

      if force == false and mapping ~= '' then
        return
      elseif mapping ~= '' then
        error(util.join({"\n",
          string.format("  Couldn't create mapping: { %s, %s, %s }", mode, lhs, rhs),
          string.format("         Existing mapping: { %s, %s, %s }", mode, lhs, mapping),
        "" }, "\n"))
      end
    end
  end

  vim.keymap.set(mode, lhs, rhs, opts)
end

util.nmap = function(...) util.map('n', ...) end
util.vmap = function(...) util.map('v', ...) end
util.imap = function(...) util.map('i', ...) end
util.cmap = function(...) util.map('c', ...) end
util.tmap = function(...) util.map('t', ...) end

util.nvomap = function(...)
  util.map({ 'n', 'v', 'o' }, ...)
end

util.unmap = vim.keymap.del

-- Create autocommand group
util.augroup = function(name)
  return vim.api.nvim_create_augroup(name, { clear = true })
end

-- Create autocommand
local augroup_default
util.autocmd = function(event, pattern, opts, command)
  opts = opts or {}

  if type(opts) ~= 'table' then
    command = opts
    opts = {}
  end

  if type(pattern) == 'table' and not vim.isarray(pattern) then
    opts = pattern
    pattern = nil
  end

  if pattern and not command then
    command = pattern
  else
    opts.pattern = pattern
  end

  if type(command) == 'function' then
    opts.callback = command
  elseif type(command) == 'string' then
    opts.command = command
  end

  augroup_default = augroup_default or util.augroup('dotfiles')
  opts.group = augroup_default

  return vim.api.nvim_create_autocmd(event, opts)
end

-- Create user command
-- https://github.com/neovim/neovim/issues/26867
util.command = function(name, command, opts, desc)
  if type(opts) == 'string' then opts = { desc = opts } end
  if type(desc) == 'string' then opts = util.merge(opts, { desc = desc }) end
  if type(desc) == 'table'  then opts = util.merge(opts, desc) end

  return vim.api.nvim_create_user_command(name, command, opts or {})
end

-- Abbreviate commands, only in command mode on first column
-- https://github.com/neovim/neovim/issues/19198
util.alias_command = function(aliases)
  for alias, command in pairs(aliases) do
    util.map('ca', alias, function()
      local space = vim.fn.stridx(vim.fn.getcmdline(), ' ')
      if vim.fn.getcmdtype() == ':' and (space == -1 or vim.fn.getcmdpos() < space) then
        return command
      else
        return alias
      end
    end, { expr = true })
  end
end

-- Return a hex color for the {key} property
-- of the {name} highlighting group.
util.get_color = function(name, key)
  local hl = vim.api.nvim_get_hl(0, { name = name })
  local color = hl and hl[key or 'fg']

  if color then
    return ('#%06x'):format(color)
  end
end

-- }}}
-- UI helpers ---------------------------------------------------------- {{{

-- Show a {message} with an optional {hl} group.
util.echo = function(message, hl)
  return vim.api.nvim_echo({{ message, hl }}, false, {})
end

-- Show an error {message}.
util.error = function(message)
  message = message:gsub('^Vim:E[0-9]*: ', '')
  return util.echo(message, 'ErrorMsg')
end

-- Show a notification {message} with optional {options}.
util.notify = function(message, opts)
  opts = util.merge({ key = message, level = 'INFO' }, opts)
  local level = vim.log.levels[opts.level]
  return require('fidget').notify(message, level, opts)
end

-- Show a notification {message} for a toggle setting.
util.notify_toggle = function(message, enabled)
  return util.notify(message, {
    annote = enabled and ' Enabled ' or ' Disabled',
    level = enabled and 'INFO' or 'WARN',
  })
end

-- Return the number of tabs.
util.tab_count = function()
  return #vim.fn.gettabinfo()
end

-- Return the number of non-floating windows in the current tab.
util.window_count = function()
  local windows = vim.api.nvim_tabpage_list_wins(0)
  return #vim.tbl_filter(function(win)
    return vim.api.nvim_win_get_config(win).relative == ''
  end, windows)
end

-- Return the number of listed buffers.
util.buffer_count = function()
  return #vim.fn.getbufinfo({ buflisted = true })
end

-- Close buffer, while keeping its window.
util.close_buffer = function(bufnr)
  bufnr = bufnr or vim.fn.bufnr()
  local window = vim.fn.bufwinid(bufnr)

  if window > -1 then
    vim.api.nvim_win_call(window, function()
      local alternate = vim.fn.bufnr('#')
      if vim.fn.buflisted(alternate) == 1 then
        vim.api.nvim_win_set_buf(0, alternate)
      else
        local state = require('bufferline.state')
        local commands = require('bufferline.commands')
        local _, item = commands.get_current_element_index(state)

        if item and item.ordinal == #state.components then
          require('bufferline').cycle(-1)
        else
          require('bufferline').cycle(1)
        end
      end
    end)
  end

  if bufnr then
    local ok, error = pcall(vim.cmd.bdelete, bufnr)
    if not ok then
      util.error(error)
    end
  end
end

-- Close window, except if it's the last normal one.
util.close_window = function(winid)
  winid = winid or 0

  if util.tab_count() > 1 and util.window_count() <= 1 then
    vim.cmd.tabclose()
    return
  end

  if vim.bo.buftype ~= 'quickfix' then
    local windows = #vim.tbl_filter(
      function(win) return vim.fn.win_gettype(win) == '' end,
      vim.fn.range(1, winnr('$'))
    )

    if windows <= 1 then return end
  end

  vim.api.nvim_win_close(winid, false)
end

-- Close tab if there's only one unnamed buffer.
util.close_tab = function()
  local name = expand('%')
  if util.buffer_count() <= 1 and util.tab_count() > 1 and (name == '' or name:match('^ministarter:')) then
    vim.cmd.tabclose()
  end
end

-- Return the window title for the current buffer
util.window_title = function()
  local filename = expand('%:t')
  local filetype = vim.bo.filetype
  local buftype = vim.bo.buftype

  -- don't show a title on startup
  if filetype == 'ministarter' or (vim.bo.bufhidden == 'hide' and filename == 'filetype-match-scratch') then
    return ''
  end

  -- use filetype as title for non-file buffers
  if filetype ~= '' and filetype ~= 'man' and (buftype == 'nofile' or buftype == 'terminal') then
    return '@' .. vim.fn.tolower(filetype)
  end

  local title = filename
  if vim.bo.modified then
    title = title .. ' ●'
  end
  if vim.bo.readonly then
    title = title .. ' 󰌾 '
  end

  return title
end

-- Return the path of the current buffer, starting with either
-- the basename of a Git repository, or the absolute path for
-- other directories ('repository/dir', '/etc/dir', '~/dir').
--
-- Optionally try to shorten the path to {max_length} characters
-- ('root/a/b/c/dir', 'root/a/…/dir', 'root/…'), and {max_depth}
-- levels of directories (defaults to 3).
util.project_path = function(max_length, max_depth)
  local path = expand('%:p:h')
    :gsub('/%.git$', '')
    :gsub('^[a-z]*:.*', '')

  if path == '' then
    path = vim.fn.getcwd()
  end

  local ok, git_dir = pcall(vim.fn.FugitiveGitDir)
  local root, name

  -- determine root and name
  if ok and git_dir ~= '' then
    root = fnamemodify(git_dir, ':p:h:h')
    name = fnamemodify(root, ':t')
  else
    path = fnamemodify(path, ':~')
    root = path:gsub([[(.)/.*]], '%1')
    name = root
  end

  -- determine relative path
  if path:sub(1, #root) == root then
    path = path:sub(#root + 1)
  end

  -- shorten relative path
  max_depth = max_depth == nil and 3 or max_depth
  local parts = util.split(path, '/')
  if #parts > max_depth then
    path = '/…/' .. util.join({ unpack(parts, #parts - max_depth + 1) }, '/')
  end

  if max_length then
    if max_length < #name then
      return name
    end

    local original = path
    if #(name .. path) > max_length then
      path = pathshorten(path)
    end

    if #(name .. path) > max_length and path:sub(1, 1) == '/' then
      path = original
      while #(name .. path) > max_length and path ~= '/' do
        path = fnamemodify(path, ':h')
      end

      if path == '/' then
        if name:sub(1, 1) == '/' then
          path = '/…'
        else
          path = ''
        end
      end
    end
  end

  return name .. path
end

-- Resize a window to its content, but at least {opts.min} or 1 lines,
-- and not more than {opts.max} lines or the height of the window.
util.resize_window = function(opts)
  opts = opts or {}
  local win = vim.o.lines - 3
  local height = util.clamp(
    line('$'),
    opts.min or 1,
    math.min(opts.max or win, win)
  )

  vim.api.nvim_win_set_height(0, height)
end

-- Toggle the quickfix or location list.
util.toggle_list = function(id)
  local windows = winnr('$')
  vim.cmd(id .. 'close')
  if windows ~= winnr('$') then
    return
  end

  if id == 'c' and #vim.fn.getqflist() == 0 then
    util.notify('Quickfix list:', { annote = 'Empty', level = 'WARN' })
    return
  elseif id == 'l' and #vim.fn.getloclist(0) == 0 then
    util.notify('Location list:', { annote = 'Empty', level = 'WARN' })
    return
  end

  local win = winnr()
  pcall(vim.cmd[id .. 'open'])

  if win ~= winnr() then
    vim.cmd.wincmd('p')
  end
end

-- Make next/previous movements repeatable
util.make_repeatable = function(...)
  return require('nvim-treesitter.textobjects.repeatable_move')
    .make_repeatable_move_pair(...)
end

-- }}}

return util
