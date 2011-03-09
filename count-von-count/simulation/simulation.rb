# encoding: utf-8

class Random
  def self.rand_between(min, max)
    min + rand * (max - min)
  end
end

class Team
  attr_reader :position, :name, :laps

  def initialize(name, min_speed = 2, max_speed = 10)
    @name = name
    @min_speed = min_speed
    @max_speed = max_speed
    @position = 0
    @laps = 0
  end

  def update(time, circuit_length)
    speed = Random.rand_between @min_speed, @max_speed
    @position += speed

    # Count laps, the simple way
    if @position > circuit_length then
      @laps += 1
      @position %= circuit_length
      $stderr.puts "Expected lap at #{time}"
    end
  end

  def to_s
    name
  end
end

class Station
  attr_reader :position

  def initialize(number, position = 0, radius = 10, accuracy = 0.8)
    @position = position
    @radius = radius
    @accuracy = accuracy
  end

  def scan(teams, circuit_length)
    teams.select do |team|
      offset = (@position - team.position) % circuit_length
      offset.abs < @radius and rand < @accuracy
    end
  end
end

class Simulation
  def initialize(teams, stations, circuit_length)
    @teams = teams
    @stations = stations
    @circuit_length = circuit_length
    @time = 0
  end

  def update(time_diff)
    # Update time
    @time += time_diff

    # Update teams
    @teams.each { |team| team.update @time, @circuit_length }

    # Search in the stations
    @stations.each do |station|
      found = station.scan @teams, @circuit_length
      found.each do |team|
        puts "#{team} #{@time} #{station.position}"
      end
    end
  end
end


def main(args = {})
  opts = { time: 2, real_time: false}.merge(args)
  # teams = ['wina', 'vtk', 'geologica', 'chemica'].collect { |n| Team.new n }
  teams = ['wina'].collect { |n| Team.new n }
  stations = (0 .. 3).collect { |x| Station.new x, x * 100 }
  circuit_length = stations.last.position + 100
  simulation = Simulation.new teams, stations, circuit_length

  1000.times do
    simulation.update opts[:time]
    sleep opts[:time] if opts[:real_time]
  end

  File.open 'expected.csv', 'w' do |file|
    teams.each do |team|
      file.write "#{team},#{team.laps}\n"
    end
  end
end

require 'optparse'

options = {}

optparse = OptionParser.new do |opts|
  
  opts.banner = "Usage: #{__FILE__} [options]"

  opts.on('-t TIME', '--time TIME', 'Set the time between values', Float) do |i|
    options[:time] = i.to_i
  end

  opts.on('-r', '--real-time', 'Run in real_time') do
    options[:real_time] = true
  end

end

unless ARGV.empty?
  optparse.parse!(ARGV)
  main(options)
else
  puts optparse
end

