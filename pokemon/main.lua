require 'team'
require 'station'

local circuitLength = 290
local function makeTeam(mac, spritePath)
  return team.Team:new(mac, love.graphics.newImage(spritePath), circuitLength)
end

local teams = {
  makeTeam('20:11:02:15:01:80', 'pokemon/bulbasaur.png', circuitLength),
  makeTeam('20:11:02:15:01:42', 'pokemon/machop.png', circuitLength),
  makeTeam('20:11:02:15:01:15', 'pokemon/mankey.png', circuitLength),
  makeTeam('20:11:02:15:01:75', 'pokemon/charmander.png', circuitLength)
}

local stationSprite = love.graphics.newImage('pokemon/station.png')
local stationFoundSprite = love.graphics.newImage('pokemon/station-found.png')
local backgroundSprite = love.graphics.newImage('pokemon/background.png')

local stations = {
  station.Station:new('00:00:00:00:01:00', 0, circuitLength),
  station.Station:new('00:00:00:00:02:00', 100, circuitLength),
  station.Station:new('00:00:00:00:03:00', 200, circuitLength),
  station.Station:new('00:00:00:00:04:00', 300, circuitLength)
}

local function drawCenteredImage(image, x, y)
  love.graphics.draw(image, x - image:getWidth() / 2, y - image:getHeight() / 2)
end

local function getPosition(position, radius)
  local width = love.graphics.getWidth()
  local height = love.graphics.getHeight()
  local centerX, centerY = width / 2, height / 2
  local radiusX, radiusY = width / 2 - 80, height / 2 - 80
  local angle = position * math.pi * 2 / circuitLength
  local x = centerX + math.cos(angle) * (radiusX + radius)
  local y = centerY + math.sin(angle) * (radiusY + radius)
  return x, y
end

local function printLaps()
  print('LAPS')
  print()
  for _, t in ipairs(teams) do
    print('  ' .. t.mac .. ': ' .. t.laps)
  end
  print()
end

function love.load()
  love.graphics.setMode(800, 600, false)
  love.keyboard.setKeyRepeat(0)
end

function love.update(dt)
  -- Update teams
  for _, t in ipairs(teams) do
    t:update(dt)
  end

  -- Update stations
  for _, s in ipairs(stations) do
    s:update(dt, teams)
  end

  -- Show laps
  if love.keyboard.isDown(' ') then
    printLaps()
  end
end

function love.draw()
  local width = love.graphics.getWidth()
  local height = love.graphics.getHeight()
  local centerX, centerY = width / 2, height / 2

  -- Draw the background
  love.graphics.draw(backgroundSprite)

  -- Draw the teams
  for _, t in ipairs(teams) do
    local x, y = getPosition(t.position, -10)
    drawCenteredImage(t.sprite, x, y)
  end

  -- Draw the stations
  for _, s in ipairs(stations) do
    local x, y = getPosition(s.position, 50)
    if s.found then
      drawCenteredImage(stationFoundSprite, x, y)
    else
      drawCenteredImage(stationSprite, x, y)
    end
  end
end
