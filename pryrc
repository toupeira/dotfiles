%w[
  yaml
  open-uri
  awesome_print
  active_support/all
].each do |lib|
  begin
    require lib
  rescue LoadError => e
    puts e.message
  end
end

puts
puts "ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE} patchlevel #{RUBY_PATCHLEVEL}} [#{RUBY_PLATFORM}]"
puts
Kernel.at_exit { puts }
