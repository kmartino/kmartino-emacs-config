#!/usr/bin/ruby
require 'uri'

NVALT_DIR = "/Users/kmarti05/workspace/autobin/__wiki"

def replace_chars(s)
    s.gsub(":","-")
end

if RUBY_VERSION.to_f > 1.9
  Encoding.default_external = Encoding::UTF_8
  Encoding.default_internal = Encoding::UTF_8
  content = STDIN.read.force_encoding('utf-8')
else
  content = STDIN.read
end

if File.dirname(ENV['MARKED_PATH']) == NVALT_DIR then
    content.gsub!(/\[\[(.*?)\]\]/) {"[#{$1}](#{URI.encode(NVALT_DIR+'/'+replace_chars($1)+"."+ENV['MARKED_EXT'])})"}
end

STDOUT.puts content
