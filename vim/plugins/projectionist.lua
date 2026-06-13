local util = require('util')

return {
  'tpope/vim-projectionist',
  event = 'LazyFile',

  config = function()
    vim.g.projectionist_heuristics = {
      ['mix.exs'] = {
        ['mix.exs'] = { type = 'mix' },
        ['config/*.exs'] = { type = 'config' },
        ['lib/mix/tasks/*.ex'] = { type = 'task' },

        ['test/*_test.exs'] = {
          type = 'test',
          alternate = 'lib/{}.ex',
        },
      },

      ['project.godot'] = {
        ['project.godot'] = { type = 'project' },
        ['*.gd'] = { type = 'class', alternate = '{}.tscn' },
        ['*.tscn'] = { type = 'scene', alternate = '{}.gd' },
        ['*.tres'] = { type = 'resource' },
      },
    }

    util.autocmd('User', 'ProjectionistDetect', function()
      local root = vim.fs.root(0, 'mix.exs')
      if not root then
        return
      end

      local name = vim.fs.basename(root)
      local lib = 'lib/' .. name .. '/'

      vim.fn['projectionist#append'](root, {
        [lib .. '*.ex'] = {
          type = 'lib',
        },
      })

      if not vim.uv.fs_stat(root .. '/deps/phoenix') then
        return
      end

      local web = 'lib/' .. name .. '_web/'
      local test_web = 'test/' .. name .. '_web/'

      vim.fn['projectionist#append'](root, {
        ['priv/static/*'] = { type = 'static' },

        ['assets/css/app.css'] = { type = 'stylesheet' },
        ['assets/css/*.css'] = { type = 'stylesheet' },

        ['assets/js/app.js'] = { type = 'javascript' },
        ['assets/js/*.js'] = { type = 'javascript' },

        [web .. 'components/*.ex'] = {
          type = 'component',
          alternate = test_web .. 'components/{}.exs',
        },

        [web .. 'controllers/*.ex'] = {
          type = 'controller',
          alternate = test_web .. 'controllers/{}_test.exs',
        },

        [web .. 'controllers/page_html/*.heex'] = {
          type = 'page',
        },

        [web .. 'router.ex'] = {
          type = 'router',
        },
      })
    end)

    util.command('AC', function()
      local confirm = vim.o.confirm
      local ok, _ = pcall(function() vim.cmd.A() end)

      if not ok then
        util.error('No alternate file')
      end

      vim.o.confirm = confirm
    end, 'Create alternate file')
  end,
}
