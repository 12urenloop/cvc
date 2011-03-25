-- team.lua
module(..., package.seeall)

require 'math'

local MIN_ACCELERATION = -5
local MAX_ACCELERATION = 5
local MIN_SPEED = 10
local MAX_SPEED = 30

Team = {}
local mt = {}
mt.__index = Team

local function bounded(a, min, max)
  if a < min then a = min end
  if a > max then a = max end
  return a
end

function Team:new(mac, sprite, circuitLength)
  local team = {}
  team.mac = mac
  team.sprite = sprite
  team.circuitLength = circuitLength
  team.position = 0
  team.speed = MIN_SPEED
  team.acceleration = 0
  team.laps = 0
  setmetatable(team, mt)
  return team
end

function Team:update(dt)
  -- Determine acceleration
  local acceleration = math.random() * 0.2
  if math.random() < 0.5 then acceleration = -acceleration end
  acceleration = self.acceleration + acceleration
  acceleration = bounded(acceleration, MIN_ACCELERATION, MAX_ACCELERATION)
  self.acceleration = acceleration

  -- Determine speed
  local speed = self.speed
  speed = speed + acceleration * dt
  speed = bounded(speed, MIN_SPEED, MAX_SPEED)
  self.speed = speed

  -- Determine position and update laps
  local position = self.position
  position = position + speed * dt
  self.laps = self.laps + math.floor(position / self.circuitLength)
  position = position % self.circuitLength
  self.position = position
end
