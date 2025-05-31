-- TODO: convert into extension
-- https://codecompanion.olimorris.dev/extending/extensions.html

local log = require('codecompanion.utils.log')

-- Store separate tasks for each chat buffer
local TASKS = {}

local STATES = {
  pending = ' ',
  done    = 'x',
  skipped = '-',
}

return {
  name = 'plan',

  opts = {
    use_handlers_once = true,
  },

  -- TODO: improve system prompt, use format from plugin tools
  system_prompt = [[
    You have access to an internal todo list where you can keep track of your tasks to achieve a goal. You can add tasks to the todo list, remove them, and mark them as done or skipped. You can also clear the list to remove all items, use this whenever you start working on a new goal.

    The updated todo list will always be displayed to the user when you make changes, so you don't have to repeat it or mention any changes that you've made.

    Only mark a task as done if you have actually finished it. When the user says something like "next" and the current task isn't done yet, continue with that task first.

    If you have access to other tools, don't start immediately calling them. First prepare a todo list and present it to the user.
  ]],

  cmds = {
    function(self, args)
      local action = args.action

      TASKS[self.chat.id] = TASKS[self.chat.id] or {}
      local tasks = TASKS[self.chat.id]

      if action == 'add' then
        if not args.text then return { status = 'error', data = 'Argument `text` is required' } end
        table.insert(tasks, { text = args.text, state = 'pending' })
      elseif action == 'remove' then
        if not args.index then return { status = 'error', data = 'Argument `index` is required' } end
        table.remove(tasks, args.index)
      elseif action == 'update' then
        if not args.index then return { status = 'error', data = 'Argument `index` is required' } end
        if not STATES[args.state] then return { status = 'error', data = 'Invalid state `' .. args.state .. '`' } end
        tasks[args.index].state = args.state
      elseif action == 'clear' then
        TASKS[self.chat.id] = nil
      else
        return { status = 'error', data = 'Invalid action `' .. action .. '`' }
      end

      return { status = 'success' }
    end
  },

  output = {
    success = function(self, agent)
      -- `for_llm` is blank because LLMs always want a tool response
      -- `for_user` is blank because we don't want to add empty lines
      -- to the output, passing an explicit empty string skips that
      agent.chat:add_tool_output(self, '', '')
    end,

    error = function(self, agent, args, stderr, _stdout)
      log:debug('[Plan Tool] Error output: %s', stderr)

      agent.chat:add_tool_output(self, string.format(
        '**Plan Tool**: There was an error running the `%s` action:\n%s',
        args.action,
        vim.iter(stderr)
          :flatten()
          :map(function(error) return '- ' .. error end)
          :join('\n')
      ))
    end,
  },

  handlers = {
    -- only render the todo list once after all tool calls
    on_exit = function(self, agent)
      local tasks = TASKS[agent.chat.id]
      if not tasks or #tasks == 0 then
        return agent.chat:add_tool_output(self, '', '')
      end

      tasks = vim.iter(ipairs(tasks))
        :map(function(index, task)
          return string.format(
            '%2d. [%s] %s',
            index,
            STATES[task.state],
            task.text
          )
        end)
        :join('\n')

      agent.chat:add_tool_output(self, 'Tasks:\n' .. tasks)
    end,
  },

  schema = {
    type = 'function',
    ['function'] = {
      name = 'plan',
      description = 'Manage an internal todo list',
      strict = true,
      parameters = {
        type = 'object',
        required = { 'action' },
        additionalProperties = false,

        properties = {
          action = {
            type = 'string',
            enum = { 'add', 'remove', 'update', 'clear' },
            description = 'The action to perform',
          },
          text = {
            type = 'string',
            description = 'The text when adding a new task',
          },
          index = {
            type = 'integer',
            description = 'The 1-based index of the task when removing or updating existing tasks',
          },
          state = {
            type = 'string',
            enum = vim.tbl_keys(STATES),
            description = 'The state when updating existing tasks',
          },
        },
      },
    },
  },
}
