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

class Ernie
  def initialize
    # Load configuration
    @config = YAML::load_file 'config.yaml'

    # Open a socket
    @socket = TCPSocket.new('localhost', @config['Listen port'])

    # Load stations
    station_map = @config['Station map']
    @stations = station_map.keys.sort_by { |station| station_map[station] }

    # Create teams
    @teams = @config['Mac set'].collect { |mac| Team.new(mac, @stations) }

    # Keep steps
    @steps = 1
  end

  def run
    # Forever aloop
    loop do
      @teams.each do |team|
        team.step
        @socket.puts "#{team.station},-,#{team.mac},-,in\n"
      end

      # Check time!
      check if @steps % 100 == 0

      sleep 0.1
      @steps += 1
    end
  end

  def get_overview
    # URI of the overview page
    rest_api = @config['Rest API']
    overwiew_url = "http://#{rest_api['Host']}:#{rest_api['Port']}/overview"
    response = Net::HTTP.get URI.parse(overwiew_url)
    JSON.parse response
  end

  def check
    puts "Checking"
    sleep 2
    overview = get_overview

    # Check every team
    @teams.each do |team|
      puts "#{team.mac}: #{team.laps} expected, #{overview[team.mac]} counted"
    end

    puts
  end
end

ernie = Ernie.new
ernie.run
