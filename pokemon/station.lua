-- station.lua
module(..., package.seeall)

require 'math'
require 'socket'

Station = {}
local mt = {}
mt.__index = Station

function Station:new(mac, position, circuitLength)
  local station = {}
  station.mac = mac
  station.position = position
  station.circuitLength = circuitLength
  station.found = false
  station.foundTime = 0

  station.socket = socket.tcp()
  station.socket:connect('localhost', 9001)

  setmetatable(station, mt)
  return station
end

function Station:signal(team)
  self.socket:send(self.mac .. ',-,' .. team.mac .. ',0\n')
end

function Station:update(dt, teams)
  self.foundTime = self.foundTime + dt

  for _, t in ipairs(teams) do
    local diff = math.abs(self.position - t.position) % self.circuitLength
    local chance = 3 * dt
    if diff < 20 and math.random() < chance then
      self.found = true
      self.foundTime = 0
      self:signal(t)
    end
  end

  if self.foundTime > 0.5 then
    self.found = false
  end
end
