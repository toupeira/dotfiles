local util = require('util')

return {
  'bootleq/vim-cycle',
  event = 'LazyFile',

  init = function()
    vim.g.cycle_no_mappings = 1
  end,

  config = function()
    util.nvomap('<C-a>', '<Plug>CycleNext')
    util.nvomap('<C-x>', '<Plug>CyclePrev')
    util.nmap('<Plug>CycleFallbackNext', '<C-a>')
    util.nmap('<Plug>CycleFallbackPrev', '<C-x>')

    vim.g.cycle_default_groups = {
      {{ '&&', '||' }},
      {{ '+', '-' }},
      {{ '+=', '-=' }},
      {{ '==', '!=' }},
      {{ '>', '<' }},
      {{ '>=', '<=' }},
      {{ 'above', 'below' }},
      {{ 'active', 'inactive' }},
      {{ 'add', 'remove' }},
      {{ 'ahead', 'behind' }},
      {{ 'and', 'or' }},
      {{ 'asc', 'desc' }},
      {{ 'assert', 'refute' }},
      {{ 'before', 'after' }},
      {{ 'credit', 'debit' }},
      {{ 'debug', 'info', 'warn', 'error', 'fatal' }},
      {{ 'enable', 'disable' }},
      {{ 'even', 'odd' }},
      {{ 'exclude', 'include' }},
      {{ 'first', 'last' }},
      {{ 'forward', 'backward' }},
      {{ 'get', 'set' }},
      {{ 'height', 'width' }},
      {{ 'horizontal', 'vertical' }},
      {{ 'in', 'out' }},
      {{ 'internal', 'external' }},
      {{ 'left', 'right' }},
      {{ 'max', 'min' }},
      {{ 'next', 'previous' }},
      {{ 'old', 'new' }},
      {{ 'only', 'except' }},
      {{ 'on', 'off' }},
      {{ 'public', 'private' }},
      {{ 'read', 'write' }},
      {{ 'show', 'hide' }},
      {{ 'staging', 'production' }},
      {{ 'to', 'from' }},
      {{ 'top', 'bottom' }},
      {{ 'true', 'false' }},
      {{ 'up', 'down' }},
      {{ 'without', 'with' }},
      {{ 'yes', 'no' }},
      {{ 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten' }},
      {{ 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday' }},
      {{ 'january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december' }},
    }

    vim.g.cycle_default_groups_for_ruby = {
      {{ 'build', 'create' }},
      {{ 'class', 'module' }},
      {{ 'else', 'elsif' }},
      {{ 'get', 'post', 'put', 'patch' }},
      {{ 'if', 'unless' }},
      {{ 'present', 'blank' }},
    }

    vim.g.cycle_default_groups_for_eruby = vim.g.cycle_default_groups_for_ruby
    vim.g.cycle_default_groups_for_haml = vim.g.cycle_default_groups_for_ruby

    vim.g.cycle_default_groups_for_gitrebase = {
      {{ 'pick', 'reword', 'edit', 'squash', 'fixup', 'drop' }},
    }
  end
}
