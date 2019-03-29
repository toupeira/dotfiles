### default gems {{{
%w[
  yaml
  active_support/all
].each do |lib|
  begin
    require lib
  rescue LoadError
    nil
  end
end
### }}}

# startup message
puts
puts "ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE} patchlevel #{RUBY_PATCHLEVEL}} [#{RUBY_PLATFORM}]"
puts
Kernel.at_exit { puts }

# delete single-letter command aliases
('a'..'z').each do |letter|
  begin
    Pry.commands.delete letter
  rescue ArgumentError
    next
  end
end

### prompt configuration {{{
cyan     = ->(text) { "\001\e[1;36m\002#{text}\001\e[0m\002" }
darkcyan = ->(text) { "\001\e[0;36m\002#{text}\001\e[0m\002" }
red      = ->(text) { "\001\e[1;31m\002#{text}\001\e[0m\002" }
gray     = ->(text) { "\001\e[1;30m\002#{text}\001\e[0m\002" }

target_string = lambda do |object|
  target = Pry.view_clip(object)
  if target != 'main'
    "#{darkcyan.call '['}#{cyan.call target}#{darkcyan.call ']'}"
  else
    ''
  end
end

separator = red.call("»")

Pry.config.prompt = [
  lambda { |object, level, pry|
    input = pry.respond_to?(:input_ring) ? pry.input_ring : pry.input_array
    "#{gray.call "[#{input.size}]"} #{Pry.config.prompt_name}#{target_string.call(object)} #{separator} "
  },
  lambda { |object, level, pry|
    input = pry.respond_to?(:input_ring) ? pry.input_ring : pry.input_array
    prompt = Pry.config.prompt_name.gsub(/\001.*?\002/, '')

    spaces = (
      "[#{input.size}]".size +
      prompt.size +
      target_string.call(object).size
    )

    "#{' ' * spaces}  #{separator} "
  }
]
### }}}
