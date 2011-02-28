#!/usr/bin/ruby

TEAMS = ['wina', 'geologica', 'chemica', 'vtk']
POSITIONS = 10

class Array
  def pick
    self[rand(length)]
  end
end

def rand_in(min, max)
  min + rand(max - min)
end

def main
  time = 0
  positions = Hash.new 0
  loop do
    team = TEAMS.pick
    positions[team] = (positions[team] + rand_in(-1, POSITIONS / 2)) % POSITIONS
    puts "#{team} #{positions[team]} #{time}" 
    time = time + rand_in(-10, 30)
  end
end

main
