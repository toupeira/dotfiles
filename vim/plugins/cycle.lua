local util = require('util')

return {
  'bootleq/vim-cycle',
  event = 'LazyFile',

  opts = {
    default = {
      { '&&', '||' },
      { '+', '-' },
      { '==', '!=' },
      { '>', '<' },

      { 'above', 'below' },
      { 'active', 'inactive' },
      { 'add', 'remove' },
      { 'ahead', 'behind' },
      { 'and', 'or' },
      { 'asc', 'desc' },
      { 'assert', 'refute' },
      { 'attach', 'detach' },
      { 'background', 'foreground' },
      { 'before', 'after' },
      { 'credit', 'debit' },
      { 'enable', 'disable' },
      { 'even', 'odd' },
      { 'exclude', 'include' },
      { 'first', 'last' },
      { 'forward', 'backward' },
      { 'get', 'set' },
      { 'height', 'width' },
      { 'horizontal', 'vertical' },
      { 'inclu', 'exclu' },
      { 'increase', 'decrease' },
      { 'internal', 'external' },
      { 'in', 'out' },
      { 'left', 'right' },
      { 'max', 'min' },
      { 'next', 'previous' },
      { 'old', 'new' },
      { 'only', 'except' },
      { 'on', 'off' },
      { 'open', 'close' },
      { 'plus', 'minus' },
      { 'public', 'private' },
      { 'read', 'write' },
      { 'show', 'hide' },
      { 'staging', 'production' },
      { 'start', 'stop' },
      { 'to', 'from' },
      { 'top', 'bottom' },
      { 'true', 'false' },
      { 'up', 'down' },
      { 'without', 'with' },
      { 'yes', 'no' },

      { 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten' },
      { 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday' },
      { 'january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december' },
      { 'debug', 'info', 'warn', 'error', 'fatal' },
    },

    filetypes = {
      aliases = {
        javascript = { 'typescript', 'vue' },
        ruby = { 'eruby', 'haml' },
      },

      gitcommit = {
        { 'feat', 'fix', 'chore', 'docs', 'revert' },
      },

      gitrebase = {
        { 'pick', 'reword', 'squash', 'fixup', 'edit', 'drop' },
      },

      javascript = {
        { 'if', 'else', 'else if' },
      },

      lua = {
        { 'if', 'else', 'elseif' },
      },

      ruby = {
        { 'if', 'unless' },
        { 'else', 'elsif' },
        { 'class', 'module' },
        { 'allow', 'expect' },
        { 'build', 'create' },
        { 'delete', 'destroy' },
        { 'get', 'post', 'put', 'patch' },
        { 'present', 'blank' },
      },

      sh = {
        { 'if', 'else', 'elif' },
      },
    },
  },

  init = function()
    vim.g.cycle_no_mappings = 1
  end,

  config = function(_, opts)
    util.nvomap('<C-a>', '<Plug>CycleNext')
    util.nvomap('<C-x>', '<Plug>CyclePrev')
    util.nmap('<Plug>CycleFallbackNext', '<C-a>')
    util.nmap('<Plug>CycleFallbackPrev', '<C-x>')

    local function map_groups(groups)
      return vim.tbl_map(function(group) return {
        group,
      } end, groups)
    end

    vim.g.cycle_default_groups = map_groups(opts.default)

    for filetype, aliases in pairs(opts.filetypes) do
      if filetype ~= 'aliases' then
        vim.g['cycle_default_groups_for_' .. filetype] = map_groups(aliases)
      end
    end

    for lang, aliases in pairs(opts.filetypes.aliases) do
      for _, alias in ipairs(aliases) do
        vim.g['cycle_default_groups_for_' .. alias] = vim.g['cycle_default_groups_for_' .. lang]
      end
    end
  end,
}
