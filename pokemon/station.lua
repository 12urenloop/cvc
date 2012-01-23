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
  setmetatable(station, mt)
  return station
end

function Station:signal(team)
  local s = socket.tcp()
  s:connect('localhost', 9001)
  s:send(self.mac .. ',-,' .. team.mac .. ',-\n')
  s:close()
end

function Station:update(dt, teams)
  self.foundTime = self.foundTime + dt

  for _, t in ipairs(teams) do
    local diff =
        (self.position + self.circuitLength - t.position) % self.circuitLength
    local chance = 3 * dt
    if math.abs(diff) < 20 and math.random() < chance then
      self.found = true
      self.foundTime = 0
      self:signal(t)
    end
  end

  if self.foundTime > 0.5 then
    self.found = false
  end
end
