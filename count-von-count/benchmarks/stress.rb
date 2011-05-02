#!/usr/bin/ruby

require 'yaml'
require 'socket'

class Team
  attr_accessor :mac

  def initialize(mac, stations)
    @mac = mac
    @stations = stations
    @position = 0
  end

  def step
    if rand() > 0.8 then
      @position += 1
      @position %= @stations.length
    end
  end

  def station
    @stations[@position]
  end
end

def main
  # Load configuration
  config = YAML::load_file 'config.yaml'

  # Open a socket
  socket = TCPSocket.new('localhost', config['Listen port'])

  # Load stations
  station_map = config['Station map']
  stations = station_map.keys.sort_by { |station| station_map[station] }

  # Create teams
  teams = config['Mac set'].collect { |mac| Team.new(mac, stations) }

  # Forever...
  loop do
    teams.each do |team|
      team.step
      socket.puts "#{team.station} #{team.mac}\n"
    end

    sleep 0.01
  end

  # Cleanup
  socket.close
end

main
