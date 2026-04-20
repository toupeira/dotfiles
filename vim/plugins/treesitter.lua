local util = require('util')
local nvomap = util.nvomap

local languages = {
  'bash',
  'c',
  'comment',
  'css',
  'diff',
  'gdscript',
  'gdshader',
  'git_config',
  'git_rebase',
  'gitattributes',
  'gitcommit',
  'gitignore',
  'html',
  'ini',
  'javascript',
  'json',
  'lua',
  'markdown',
  'markdown_inline',
  'mermaid',
  'python',
  'regex',
  'ruby',
  'scss',
  'toml',
  'typescript',
  'vim',
  'vimdoc',
  'vue',
  'yaml',
  'xml',
}

local map_select = function(key, query, desc)
  return {
    key,
    mode = { 'x', 'o' },
    '<Cmd>lua require("nvim-treesitter-textobjects.select").select_textobject("' .. query .. '", "textobjects")<CR>',
    desc = desc,
  }
end

local map_move = function(key, query, action, desc)
  return {
    key,
    mode = { 'n', 'x', 'o' },
    '<Cmd>lua require("nvim-treesitter-textobjects.move").' .. action .. '("' .. query .. '", "textobjects")<CR>',
    desc = desc,
  }
end

return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',

  dependencies = {
    { 'RRethy/nvim-treesitter-endwise' },

    { 'nvim-treesitter/nvim-treesitter-textobjects',
      keys = {
        map_select('am', '@class.outer', 'Select current module'),
        map_select('im', '@class.inner', 'Select current module body'),
        map_select('af', '@function.outer', 'Select current function'),
        map_select('if', '@function.inner', 'Select current function body'),

        map_move(']a', '@parameter.inner', 'goto_next_start', 'Next argument'),
        map_move('[a', '@parameter.inner', 'goto_previous_start', 'Previous argument'),
        map_move(']A', '@parameter.inner', 'goto_next_end', 'End of argument'),
        map_move('[A', '@parameter.inner', 'goto_previous_end', 'End of previous argument'),

        map_move(']m', '@class.outer', 'goto_next_start', 'Next module'),
        map_move('[m', '@class.outer', 'goto_previous_start', 'Previous module'),
        map_move(']M', '@class.outer', 'goto_next_end', 'End of module'),
        map_move('[M', '@class.outer', 'goto_previous_end', 'End of previous module'),

        map_move(']f', '@function.outer', 'goto_next_start', 'Next function'),
        map_move('[f', '@function.outer', 'goto_previous_start', 'Previous function'),
        map_move(']F', '@function.outer', 'goto_next_end', 'End of function'),
        map_move('[F', '@function.outer', 'goto_previous_end', 'End of previous function'),
      },
      opts = {
        select = { lookahead = true },
      },
    },

    { 'nvim-treesitter/nvim-treesitter-context',
      opts = {
        max_lines = 3,
        min_window_height = 10,
        multiwindow = true,
      },
    },

    { 'aaronik/treewalker.nvim',
      init = function()
        local function move(action)
          return function()
            vim.cmd.Treewalker(action)
            pcall(vim.cmd.foldopen)
          end
        end

        local descendant, ancestor = util.make_repeatable(move('Right'), move('Left'))
        local next, previous = util.make_repeatable(move('Down'), move('Up'))

        nvomap(']s', descendant, 'Move to descendant node')
        nvomap('[s', ancestor, 'Move to ancestor node')
        nvomap(']S', next, 'Move to next neighbor node')
        nvomap('[S', previous, 'Move to previous neighbor node')
      end
    },
  },

  config = function()
    if not util.is_sudo then
      require('nvim-treesitter').install(languages)
    end
  end,
}
