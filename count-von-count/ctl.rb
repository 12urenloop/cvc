#!/usr/bin/ruby

require 'yaml'
require 'socket'

# Send some data
def send(data)
  config = YAML::load_file 'config.yaml'
  socket = TCPSocket.new('localhost', config['Listen port'])
  socket.write data
  # sleep 3
  socket.close
end

# Usage
def usage_info
  info=<<-HERE
#{$0} reset mac

  Reset current data for the given mac address
  HERE
end

# Main function
def main
  if ARGV.length == 0 then
    puts usage_info
  else
    case ARGV[0]
    when 'reset'
      send "RESET,#{ARGV[1]}\r\n"
    end
  end
end

# Call main
main
