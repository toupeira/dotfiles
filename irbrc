# vim: ft=ruby

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

puts
puts "ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE} patchlevel #{RUBY_PATCHLEVEL}} [#{RUBY_PLATFORM}]"
puts
Kernel.at_exit { puts }

if defined? IRB
  IRB.conf[:AUTO_INDENT] = true
  IRB.conf[:IRB_RC] = proc do |conf|
    prompt = "#{conf.irb_name}[#{conf.workspace.main}] "
    leader = ' ' * prompt.size
    conf.prompt_i = '%N[%m] >>> '
    conf.prompt_s = leader + ' .. '
    conf.prompt_n = conf.prompt_c = '%N[%m]  .. '
    conf.return_format = " :: %s\n"
    print "\033]0;#{prompt}\007"
  end
end

def ls(*args)
  ls = system('which gls &>/dev/null') ? 'gls' : 'ls'
  system("#{ls} --color #{args.join ' '}")
end

def cd(dir = ENV['HOME'])
  Dir.chdir(dir.to_s) && ls
end

class Object
  def methods?
    methods.sort - Object.methods
  end
end
