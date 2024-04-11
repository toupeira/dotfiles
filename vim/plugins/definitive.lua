local util = require('util')
local nmap = util.nmap

return {
  'misterbuckley/vim-definitive',
  cmd = { 'FindDefinition', 'SFindDefinition' },

  init = function()
    nmap('gd', { 'FindDefinition', 'normal zvzz' }, 'Go to definition')
    nmap('gD', { 'SFindDefinition', 'normal zvzz' }, 'Go to definition in split')
  end,

  config = function()
    vim.g.definitive_associated_filetypes = {
      eruby = 'ruby',
      haml = 'ruby',
      vue = 'javascript',
      zsh = 'sh',
    }

    vim.g.definitive_definitions = {
      lua = [[\(\(\<\([^=]\+,\s*\)\?\|\.\)%1\(\s*,[^=]\+\)\?\s*=\|\<function\s\+\(\w\+\.\)\?%1\>\)]],
      ruby = vim.fn.join({
        [[\<\(]],
          [[\(def\|class\|module\|alias\)\s\+\(self\.\)\=%1\>\|]],
          [[%1\s*=\|]],
          [[\<\(]],
            [[alias_method\|attr_reader\|attr_accessor\|]],
            [[delegate\|]],
            [[attribute\|serialize\|]],
            [[scope\|has_one\|has_many\|has_and_belongs_to_many\|belongs_to\|]],
            [[has_one_attached]],
          [[\) :%1\>]],
        [[\)]],
      }, '')
    }
  end
}
