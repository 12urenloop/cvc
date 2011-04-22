#!/usr/bin/ruby

require 'yaml'
require 'socket'
require 'json'
require 'net/http'

class Team
  attr_accessor :mac, :laps

  def initialize(mac, stations)
    @mac = mac
    @stations = stations
    @position = 0
    @laps = 0
  end

  def step
    if rand() > 0.8 then
      @position += 1

      # Lap
      if @position >= @stations.length then
        @position = 0
        @laps += 1
      end
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

  # URI of the overview page
  rest_api = config['Rest API']
  overwiew_url = "http://#{rest_api['Host']}:#{rest_api['Port']}/overview"

  # Keep steps
  steps = 0

  # Forever...
  loop do
    teams.each do |team|
      team.step
      socket.puts "#{team.station} #{team.mac}\n"
    end

    # Check time!
    if steps % 10000 == 0 then
      puts "Checking"

      sleep 2
      response = Net::HTTP.get URI.parse(overwiew_url)
      overview = JSON.parse response

      # Check every team
      teams.each do |team|
        puts "#{team.mac}: #{team.laps} expected, #{overview[team.mac]} counted"
      end

      puts
    end

    sleep 0.001
    steps += 1
  end

  # Cleanup
  socket.close
end

main
