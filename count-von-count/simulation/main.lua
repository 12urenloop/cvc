require 'team'
require 'station'

local circuitRadius = 220
local circuitLength = 400
local function makeTeam(mac, spritePath)
  return team.Team:new(mac, love.graphics.newImage(spritePath), circuitLength)
end

local teams = {
  makeTeam('00:00:00:00:00:01', 'pokemon/bulbasaur.png', circuitLength),
  makeTeam('00:00:00:00:00:02', 'pokemon/machop.png', circuitLength),
  makeTeam('00:00:00:00:00:03', 'pokemon/mankey.png', circuitLength),
  makeTeam('00:00:00:00:00:04', 'pokemon/charmander.png', circuitLength)
}

local stationSprite = love.graphics.newImage('pokemon/station.png')
local stationFoundSprite = love.graphics.newImage('pokemon/station-found.png')
local backgroundSprite = love.graphics.newImage('pokemon/background.png')

local stations = {
  station.Station:new('Station-0', 0, circuitLength),
  station.Station:new('Station-1', 100, circuitLength),
  station.Station:new('Station-2', 200, circuitLength),
  station.Station:new('Station-3', 300, circuitLength)
}

local function drawCenteredImage(image, x, y)
  love.graphics.draw(image, x - image:getWidth() / 2, y - image:getHeight() / 2)
end

local function getPosition(position, radius)
  local width = love.graphics.getWidth()
  local height = love.graphics.getHeight()
  local centerX, centerY = width / 2, height / 2
  local angle = position * math.pi * 2 / circuitLength
  local x = centerX + math.cos(angle) * (circuitRadius + radius)
  local y = centerY + math.sin(angle) * (circuitRadius + radius)
  return x, y
end

function love.load()
  -- love.graphics.toggleFullscreen()
  love.graphics.setMode(800, 600, false)
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
end

function love.draw()
  local width = love.graphics.getWidth()
  local height = love.graphics.getHeight()
  local centerX, centerY = width / 2, height / 2

  -- Draw the background
  love.graphics.draw(backgroundSprite)

  -- Draw the circuit
  love.graphics.circle('line', centerX, centerY, circuitRadius, 50)

  -- Draw the teams
  for _, t in ipairs(teams) do
    local x, y = getPosition(t.position, 0)
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
