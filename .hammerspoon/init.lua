-------------------------------------------------------------------------------
-- Reload configuration
-------------------------------------------------------------------------------
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
	hs.reload()
	hs.alert.show("Config loaded")
end)

-------------------------------------------------------------------------------
-- Deactivate Wifi (Discipline!)
-------------------------------------------------------------------------------
local function trim(s)
  return s:match'^%s*(.*%S)' or ''
end

local function getWifiInterface()
	local command = "/usr/sbin/networksetup -listallhardwareports | /usr/bin/awk '$3==\"Wi-Fi\" {getline; print $2}'"
	command = io.popen(command, 'r')
	local interfaceName = command:read('*all')
	return trim(interfaceName)
end

local function enableNetworkInterface(name, status)
	local command = string.format("/usr/sbin/networksetup -setairportpower %s off", interface, status and "on" or "off")
	os.execute(command)
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "D", function()
	local interface = getWifiInterface()
	enableNetworkInterface(interface, false)
end)


-------------------------------------------------------------------------------
-- Maximize & Restore windows
-------------------------------------------------------------------------------
local maxWindowOrigFrame = {}
local function maximizeOrRestoreWindow(window)
	forceMaximize = forceMaximize or false
	local origFrame = maxWindowOrigFrame[window:id()]

	if forceMaximize ~= true and origFrame ~= nil then
		window:setFrame(origFrame)
		maxWindowOrigFrame[window:id()] = nil
	else
		maxWindowOrigFrame[window:id()] = window:frame()
		window:maximize()
	end
end


hs.hotkey.bind({"cmd", "alt", "ctrl"}, "M", function()
	local focusedWindow = hs.window.focusedWindow()
	maximizeOrRestoreWindow(focusedWindow)
end)


-------------------------------------------------------------------------------
-- Grid layout
-------------------------------------------------------------------------------
size_stops = {0.5, 0.33, 0.25}

function shallowcopy(orig)
  local orig_type = type(orig)
  local copy
  if orig_type == 'table' then
    copy = {}
    for orig_key, orig_value in pairs(orig) do
      copy[orig_key] = orig_value
    end
  else
    copy = orig
  end
  return copy
end

function round(num, numDecimalPlaces)
  local mult = 10^(numDecimalPlaces or 0)
  return math.floor(num * mult + 0.5) / mult
end

local function contains(tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return true
    end
  end

  return false
end

local function indexof(tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return index
    end
  end

  return -1
end

-- Given a list of items to cycle through and the current value, return the next item in the list
function cycleItems(items, val)
  local idx = indexof(items, val)
  if(idx == -1 or idx + 1 > #items) then
    return items[1]
  else
    return items[idx + 1]
  end
end

local function widthUpdateToScreenFraction(window, fraction)
	local screen = window:screen()
	local fullFrame = screen:frame()
	local windowFrame = window:frame()
	local newFrame = {}

	newFrame.y = windowFrame.y
	newFrame.h = windowFrame.h

	newFrame.x = windowFrame.x
	newFrame.w = fullFrame.w * fraction

	window:setFrame(newFrame)
end

local function frameWidthUpdateToScreenFraction(frame, fraction)
	local fullFrame = window:screen():frame()
	frame.w = fullFrame.w * fraction
end

local function frameHeightUpdateToScreenFraction(frame, fraction)
	local fullFrame = window:screen():frame()
	frame.h = fullFrame.h * fraction
end

-- local function windowHeightUpdateToScreenFraction(window, fraction)
-- 	local screen = window:screen()
-- 	local fullFrame = screen:frame()
-- 	local windowFrame = window:frame()
-- 	local newFrame = {}

-- 	newFrame.y = windowFrame.y
-- 	newFrame.h = fullFrame.h * fraction

-- 	newFrame.x = windowFrame.x
-- 	newFrame.w = windowFrame.w

-- 	window:setFrame(newFrame)
-- end


local function widthScreenFraction(window, fraction)
	local fullFrame = window:screen():frame()
	local windowFrame = window:frame()
  local fraction = round(windowFrame.w / fullFrame.w, 2)
  return fraction
end

local function heightScreenFraction(window, fraction)
	local fullFrame = window:screen():frame()
	local windowFrame = window:frame()
  local fraction = round(windowFrame.h / fullFrame.h, 2)
  return fraction
end

function gridInfoFromWindow(window)
  local widthFraction = widthScreenFraction(window)
  local heightFraction = heightScreenFraction(window)
  local rowCount = math.floor(1.0/heightFraction)
  local columnCount = math.floor(1.0/widthFraction)
	local fullFrame = window:screen():frame()
	local windowFrame = window:frame()
  local cellHeight = round(fullFrame.h / rowCount, 0)
  local cellWidth = round(fullFrame.w / columnCount, 0)

  return {row = math.floor(windowFrame.y / cellHeight),
          column = math.floor(windowFrame.x / cellWidth),
          rowCount = rowCount,
          columnCount = columnCount,
          cellWidth = cellWidth,
          cellHeight = cellHeight}
end

function frameFromGridInfo(gridInfo)
	local fullFrame = hs.window.focusedWindow():screen():frame()
  return {x = gridInfo.cellWidth * gridInfo.column,
          y = gridInfo.cellHeight * gridInfo.row,
          w = gridInfo.cellWidth,
          h = gridInfo.cellHeight}
end

function frameAlignLeft(ref, target)
  target.x = ref.x
end

function frameAlignTop(ref, target)
  target.y = ref.y
end

function frameAlignRight(ref, target)
  target.x = ref.x + ref.w - target.w
end

function frameAlignBottom(ref, target)
  target.y = ref.y + ref.h - target.h
end

function frameMatchWidth(ref, target)
  target.w = ref.w
end

function frameMatchHeight(ref, target)
  target.h = ref.h
end

local function frameSetWidthToReference(ref, target, fraction)
  target.w = ref.w * fraction
end

local function frameSetHeightToReference(ref, target, fraction)
  target.h = ref.h * fraction
end

local function frameRightMatches(ref, target)
  return ref.x + ref.w == target.x + target.w
end

local function frameLeftMatches(ref, target)
  return ref.x == target.x
end

local function frameCenterX(ref, target)
	target.x = (ref.w - target.w)/2
end

-------------------------------------------------------------------------------
-- Windows operations
-------------------------------------------------------------------------------

local function centerToScreen(window)
	local fullFrame = window:screen():frame()
	local newFrame = window:frame()

  frameAlignTop(fullFrame, newFrame)
  frameCenterX(fullFrame, newFrame)

	window:setFrame(newFrame)
end

local function fullHeight(window)
	local fullFrame = window:screen():frame()
	local newFrame = window:frame()

  frameAlignTop(fullFrame, newFrame)
  frameMatchHeight(fullFrame, newFrame)

	window:setFrame(newFrame)
end

local function fwindowMoveToCenter()
	local focusedWindow = hs.window.focusedWindow()
	centerToScreen(focusedWindow)
end

local function fwindowFullHeight()
	local focusedWindow = hs.window.focusedWindow()
	fullHeight(focusedWindow)
end

local function fwindowLeftHalf()
    local window = hs.window.focusedWindow()
    local newFrame = hs.geometry.copy(window:frame())
    local fullFrame = window:screen():frame()

    frameSetWidthToReference(fullFrame, newFrame, 0.5)
    frameMatchHeight(fullFrame, newFrame)
    frameAlignLeft(fullFrame, newFrame)
    window:setFrame(newFrame)
end

local function fwindowRightHalf()
    local window = hs.window.focusedWindow()
    local newFrame = hs.geometry.copy(window:frame())
    local fullFrame = window:screen():frame()

    frameSetWidthToReference(fullFrame, newFrame, 0.5)
    frameMatchHeight(fullFrame, newFrame)
    frameAlignRight(fullFrame, newFrame)
    window:setFrame(newFrame)
end

-------------------------------------------------------------------------------
-- Key bindings
-------------------------------------------------------------------------------
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "HOME", function()
    fwindowMoveToCenter()
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "END", function()
    fwindowFullHeight()
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "help", function()
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "PageUp", function()
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "forwarddelete", function()
    fwindowLeftHalf()
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "PageDown", function()
    fwindowRightHalf()
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "left", function()
    -- Move the focused window to the next cell to the left

    local window = hs.window.focusedWindow()

    -- What kind of grid is the window on right now?
    local gridInfo = gridInfoFromWindow(window)

    -- Move to the previous column if possible
    if gridInfo.column ~= 0 then
      gridInfo.column = math.max(gridInfo.column - 1, 0)
      window:setFrame(frameFromGridInfo(gridInfo))

    else
      -- We're already at the left edge of the screen
      -- Cycle the window size instead
      local newFrame = window:frame()
      frameSetWidthToReference(
        window:screen():frame(),
        newFrame,
        cycleItems(size_stops, widthScreenFraction(window)));

      window:setFrame(newFrame)
    end

end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "right", function()
    local window = hs.window.focusedWindow()
    local gridInfo = gridInfoFromWindow(window)

    if gridInfo.column ~= gridInfo.columnCount - 1 then
      gridInfo.column = math.min(gridInfo.column + 1, gridInfo.columnCount - 1)
      window:setFrame(frameFromGridInfo(gridInfo))

    else
      local newFrame = window:frame()
      local fullFrame = window:screen():frame()
      frameSetWidthToReference(
        fullFrame,
        newFrame,
        cycleItems(size_stops, widthScreenFraction(window)));

      -- Since we're squishing the window towards a screen edge,
      -- it makes sense to continue aligning to that edge
      frameAlignRight(fullFrame, newFrame);
      window:setFrame(newFrame)
    end
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "up", function()
    local window = hs.window.focusedWindow()
    local gridInfo = gridInfoFromWindow(window)

    -- Move to the a row up if possible
    if gridInfo.row ~= 0 then
      gridInfo.row = math.max(gridInfo.row - 1, 0)
      window:setFrame(frameFromGridInfo(gridInfo))

    else
      local newFrame = window:frame()
      local fullFrame = window:screen():frame()
      frameSetHeightToReference(
        fullFrame,
        newFrame,
        cycleItems(size_stops, heightScreenFraction(window)));

      window:setFrame(newFrame)
    end
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "down", function()
    local window = hs.window.focusedWindow()
    local gridInfo = gridInfoFromWindow(window)

    if gridInfo.row ~= gridInfo.rowCount - 1 then
      gridInfo.row = math.min(gridInfo.row + 1, gridInfo.rowCount - 1)
      window:setFrame(frameFromGridInfo(gridInfo))

    else
      local newFrame = window:frame()
      local fullFrame = window:screen():frame()
      frameSetHeightToReference(
        fullFrame,
        newFrame,
        cycleItems(size_stops, heightScreenFraction(window)));

      -- Since we're squishing the window towards a screen edge,
      -- it makes sense to continue aligning to that edge
      frameAlignBottom(fullFrame, newFrame);
      window:setFrame(newFrame)
    end
end)
