# vim: ft=ruby

require 'rubygems'
require 'yaml'
require 'ostruct'
require 'open-uri'
require 'wirble'
require 'active_support/all'

puts
puts `ruby -v`
puts

Wirble.init :init_colors => true

class Object
  def methods?
    self.methods.sort - Object.methods
  end
end

def ls(*args)
  system("ls --color #{args.join ' '}")
end

def cd(dir=ENV['HOME'])
  Dir.chdir(dir.to_s) and ls
end

Kernel::at_exit { puts }

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:IRB_RC] = proc do |conf|
  prompt = "#{conf.irb_name}[#{conf.workspace.main}] "
  leader = " " * prompt.size
  conf.prompt_i = "%N[%m] >>> "
  conf.prompt_s = leader + " .. "
  conf.prompt_n = conf.prompt_c = "%N[%m]  .. "
  conf.return_format = " :: %s\n"
  print "]0;#{prompt}\007"
end

# MethodFinder
require 'stringio'

class Object
  # Clone fails on numbers, but they're immutable anyway
  def megaClone
    begin self.clone; rescue; self; end
  end

  def what?(*a)
    MethodFinder.new(self, *a)
  end
end

class MethodFinder
  def initialize( obj, *args )
    @obj = obj
    @args = args
  end

  def ==( val )
    MethodFinder.show( @obj, val, *@args)
  end

  # Find all methods on [anObject] which, when called with [args] return [expectedResult]
  def self.find( anObject, expectedResult, *args )
    anObject.methods.select { |name| anObject.method(name).arity == args.size }.
                     select { |name| begin anObject.megaClone.method( name ).call(*args) == expectedResult;
                                     rescue; end }
  end

  # Pretty-prints the results of the previous method
  def self.show( anObject, expectedResult, *args )
    $old_stderr = $stderr; $stderr = StringIO.new
    methods =
      find( anObject, expectedResult, *args ).each { |name|
        print "#{anObject.inspect}.#{name}"
        print "(" + args.map { |o| o.inspect }.join(", ") + ")" unless args.empty?
        puts " == #{expectedResult.inspect}"
      }
    $stderr = $old_stderr
    methods
  end
end
