# frozen_string_literal: true

# default gems
%w[
  yaml
  active_support/all
].each do |lib|
  begin
    require lib
  rescue LoadError
    puts "Couldn't load #{lib}"
  end
end

lambda {
  gray     = ->(text) { "\001\e[2m\002#{text}\001\e[0m\002" }
  darkcyan = ->(text) { "\001\e[0;36m\002#{text}\001\e[0m\002" }
  red      = ->(text) { "\001\e[1;31m\002#{text}\001\e[0m\002" }
  magenta  = ->(text) { "\001\e[1;35m\002#{text}\001\e[0m\002" }
  cyan     = ->(text) { "\001\e[1;36m\002#{text}\001\e[0m\002" }

  # startup message
  puts
  puts gray.call("ruby #{RUBY_VERSION}p#{RUBY_PATCHLEVEL}")
  puts
  Kernel.at_exit { puts }

  # prompt configuration
  break if Pry::VERSION < '0.13'

  prompt = lambda { |object, _level, pry|
    object_name = Pry.view_clip(object)
    object_name =
      if object_name == 'main'
        ''
      else
        "#{darkcyan.call '['}#{cyan.call(object_name)}#{darkcyan.call ']'}"
      end

    "#{gray.call "[#{pry.input_ring.size}]"} #{Pry.config.prompt_name}#{object_name}#{red.call('>')} "
  }

  Pry.config.prompt = Pry::Prompt.new(
    'custom', nil,
    [
      prompt,

      lambda { |*args|
        spaces = prompt.call(*args).gsub(/(\e\[.*?m|[^[:print:]])/, '').size - 2
        "#{' ' * spaces}#{magenta.call('>')} "
      },
    ]
  )
}.call

# vim: ft=ruby
