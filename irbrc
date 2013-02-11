# -*- mode: ruby -*-
require 'rubygems'
require 'irb/completion'

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:PROMPT_MODE] = :SIMPLE
TERM = ENV['TERM']

begin
  require 'wirble'
rescue LoadError => err
  $stderr.puts "Couldn't load Wirble: #{err}"
end

if defined?(Wirble)
  Wirble.init
  Wirble.colorize # unless TERM == "dumb"
end
