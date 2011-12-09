# vim: ft=ruby

begin
  require 'rubygems'
  require 'yaml'
  require 'ostruct'
  require 'open-uri'
  require 'wirble'
  require 'active_support/all'
rescue LoadError => e
  puts e.message
  exit
end

puts
puts `ruby -v`
puts
Kernel::at_exit { puts }

Wirble.init :init_colors => true

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:IRB_RC] = proc do |conf|
  prompt = "#{conf.irb_name}[#{conf.workspace.main}] "
  leader = " " * prompt.size
  conf.prompt_i = "%N[%m] >>> "
  conf.prompt_s = leader + " .. "
  conf.prompt_n = conf.prompt_c = "%N[%m]  .. "
  conf.return_format = " :: %s\n"
  print "\033]0;#{prompt}\007"
end

def ls(*args)
  ls = system('which gls &>/dev/null') ? 'gls' : 'ls'
  system("#{ls} --color #{args.join ' '}")
end

def cd(dir=ENV['HOME'])
  Dir.chdir(dir.to_s) and ls
end

class Object
  def methods?
    self.methods.sort - Object.methods
  end
end
