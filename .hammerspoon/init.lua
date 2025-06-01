-------------------------------------------------------------------------------
-- Reload configuration
-------------------------------------------------------------------------------
hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "R", function()
	hs.reload()
	hs.alert.show("Config loaded")
end)

-------------------------------------------------------------------------------
-- Deactivate Wifi (Discipline!)
-------------------------------------------------------------------------------
local function trim(s)
	return s:match("^%s*(.*%S)") or ""
end

local function getWifiInterface()
	local command = "/usr/sbin/networksetup -listallhardwareports | /usr/bin/awk '$3==\"Wi-Fi\" {getline; print $2}'"
	command = io.popen(command, "r")
	local interfaceName = command:read("*all")
	return trim(interfaceName)
end

local function enableNetworkInterface(name, status)
	local command = string.format("/usr/sbin/networksetup -setairportpower %s off", interface, status and "on" or "off")
	os.execute(command)
end

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "D", function()
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

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "M", function()
	local focusedWindow = hs.window.focusedWindow()
	maximizeOrRestoreWindow(focusedWindow)
end)

-------------------------------------------------------------------------------
-- Grid layout
-------------------------------------------------------------------------------
size_stops = { 0.5, 0.33, 0.25 }

function shallowcopy(orig)
	local orig_type = type(orig)
	local copy
	if orig_type == "table" then
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
	local mult = 10 ^ (numDecimalPlaces or 0)
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
	if idx == -1 or idx + 1 > #items then
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
--	local screen = window:screen()
--	local fullFrame = screen:frame()
--	local windowFrame = window:frame()
--	local newFrame = {}

--	newFrame.y = windowFrame.y
--	newFrame.h = fullFrame.h * fraction

--	newFrame.x = windowFrame.x
--	newFrame.w = windowFrame.w

--	window:setFrame(newFrame)
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
	local rowCount = math.floor(1.0 / heightFraction)
	local columnCount = math.floor(1.0 / widthFraction)
	local fullFrame = window:screen():frame()
	local windowFrame = window:frame()
	local cellHeight = round(fullFrame.h / rowCount, 0)
	local cellWidth = round(fullFrame.w / columnCount, 0)

	return {
		row = round(windowFrame.y / cellHeight, 0),
		column = round(windowFrame.x / cellWidth, 0),
		rowExact = windowFrame.y / cellHeight,
		columnExact = windowFrame.x / cellWidth,
		rowCount = rowCount,
		columnCount = columnCount,
		cellWidth = cellWidth,
		cellHeight = cellHeight,
	}
end

function frameFromGridInfo(gridInfo)
	local fullFrame = hs.window.focusedWindow():screen():frame()
	return {
		x = gridInfo.cellWidth * gridInfo.column,
		y = gridInfo.cellHeight * gridInfo.row,
		w = gridInfo.cellWidth,
		h = gridInfo.cellHeight,
	}
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
	target.x = (ref.w - target.w) / 2
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
hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "HOME", function()
	fwindowMoveToCenter()
end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "END", function()
	fwindowFullHeight()
end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "help", function() end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "PageUp", function() end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "forwarddelete", function()
	fwindowLeftHalf()
end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "PageDown", function()
	fwindowRightHalf()
end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "left", function()
	-- Move the focused window to the next cell to the left

	local window = hs.window.focusedWindow()

	-- What kind of grid is the window on right now?
	local gridInfo = gridInfoFromWindow(window)

	-- If the window isn't on the grid, we'll want to re-interpret which the cell it is in
	if gridInfo.columnExact % 1 > 0.1 then
		gridInfo.column = math.ceil(gridInfo.columnExact)
	end

	-- Move to the previous column if possible
	if gridInfo.column ~= 0 then
		gridInfo.column = math.max(gridInfo.column - 1, 0)
		window:setFrame(frameFromGridInfo(gridInfo))
	else
		-- We're already at the left edge of the screen
		-- Cycle the window size instead
		local newFrame = window:frame()
		frameSetWidthToReference(window:screen():frame(), newFrame, cycleItems(size_stops, widthScreenFraction(window)))

		window:setFrame(newFrame)
	end
end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "right", function()
	local window = hs.window.focusedWindow()
	local gridInfo = gridInfoFromWindow(window)

	if gridInfo.columnExact % 1 > 0.1 then
		gridInfo.column = math.floor(gridInfo.columnExact)
	end

	if gridInfo.column ~= gridInfo.columnCount - 1 then
		gridInfo.column = math.min(gridInfo.column + 1, gridInfo.columnCount - 1)
		window:setFrame(frameFromGridInfo(gridInfo))
	else
		local newFrame = window:frame()
		local fullFrame = window:screen():frame()
		frameSetWidthToReference(fullFrame, newFrame, cycleItems(size_stops, widthScreenFraction(window)))

		-- Since we're squishing the window towards a screen edge,
		-- it makes sense to continue aligning to that edge
		frameAlignRight(fullFrame, newFrame)
		window:setFrame(newFrame)
	end
end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "up", function()
	local window = hs.window.focusedWindow()
	local gridInfo = gridInfoFromWindow(window)

	if gridInfo.rowExact % 1 > 0.1 then
		gridInfo.row = math.ceil(gridInfo.rowExact)
	end

	-- Move to the a row up if possible
	if gridInfo.row ~= 0 then
		gridInfo.row = math.max(gridInfo.row - 1, 0)
		window:setFrame(frameFromGridInfo(gridInfo))
	else
		local newFrame = window:frame()
		local fullFrame = window:screen():frame()
		frameSetHeightToReference(fullFrame, newFrame, cycleItems(size_stops, heightScreenFraction(window)))

		window:setFrame(newFrame)
	end
end)

hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "down", function()
	local window = hs.window.focusedWindow()
	local gridInfo = gridInfoFromWindow(window)

	if gridInfo.rowExact % 1 > 0.1 then
		gridInfo.row = math.floor(gridInfo.rowExact)
	end

	if gridInfo.row ~= gridInfo.rowCount - 1 then
		gridInfo.row = math.min(gridInfo.row + 1, gridInfo.rowCount - 1)
		window:setFrame(frameFromGridInfo(gridInfo))
	else
		local newFrame = window:frame()
		local fullFrame = window:screen():frame()
		frameSetHeightToReference(fullFrame, newFrame, cycleItems(size_stops, heightScreenFraction(window)))

		-- Since we're squishing the window towards a screen edge,
		-- it makes sense to continue aligning to that edge
		frameAlignBottom(fullFrame, newFrame)
		window:setFrame(newFrame)
	end
end)

--------------------------------------------------------------------------------
-- Custom key handling
--
-- We watch for the keydown event and match against some keycode to triggered
-- the handler.
--------------------------------------------------------------------------------

local function keyDispatch(event)
	return false
end

-- Stop the keyboard event watcher if it has already been defined
-- This allows config reloading without stacking watchers
if keyboardTap ~= nil then
	keyboardTap:stop()
end

local keyboardTap = hs.eventtap.new({ hs.eventtap.event.types.keyDown }, function(event)
	if keyDispatch(event) then
		return true
	else
		-- hs.printf("key down!")
		-- hs.printf(hs.inspect.inspect(event:getFlags()))
		-- hs.printf(hs.inspect.inspect(event:getKeyCode()))
		return false
	end
end)

keyboardTap:start()

local function clone(t) -- deep-copy a table
	if type(t) ~= "table" then
		return t
	end
	local meta = getmetatable(t)
	local target = {}
	for k, v in pairs(t) do
		if type(v) == "table" then
			target[k] = clone(v)
		else
			target[k] = v
		end
	end
	setmetatable(target, meta)
	return target
end

local function flashRect(rect)
	local obj = hs.drawing.rectangle(rect)

	obj:show(0.5)
	hs.timer.doAfter(1, function()
		obj:hide(0.5)
		hs.timer.doAfter(0.5, function()
			obj:delete()
		end)
	end)
end

local function rectTop(rect)
	return rect.y
end

local function rectBottom(rect)
	return rect.y + rect.h
end

local function rectLeft(rect)
	return rect.x
end

local function rectRight(rect)
	return rect.x + rect.w
end

local function rectSetRight(rect, value)
	rect.x = value - rect.w
	return rect
end

local function rectSetBottom(rect, value)
	rect.y = value - rect.h
	return rect
end

local function rectCenterX(rect, value)
	return rect.x + rect.w / 2
end

local function rectCenterY(rect, value)
	return rect.y + rect.h / 2
end

local function rectSetCenterX(rect, value)
	rect.x = value - rect.w / 2
	return rect
end

local function rectSetCenterY(rect, value)
	rect.y = value - rect.h / 2
	return rect
end

local function rectAlignY(rect, value, alignment)
	if alignment == "center" then
		return rectSetCenterY(rect, value)
	elseif alignment == "top" then
		return rectSetTop(rect, value)
	elseif alignment == "bottom" then
		return rectSetBottom(rect, value)
	else
		return rect
	end
end

-- Generates all rectangles that represents snappable hotspots
local function generateSnapHotspots()
	local screenFrame = hs.window.focusedWindow():screen():frame()

	local corner_size = 10
	local ann_size = 10
	rects = {}

	local rectPrototype = { x = screenFrame.x, y = screenFrame.y, w = corner_size, h = corner_size }

	-- Corners
	-- These are supposed to snap the window to quarter screen size
	rects.top_left = clone(rectPrototype)

	rects.top_right = rectSetRight(clone(rectPrototype), rectRight(screenFrame))

	rects.bottom_left = rectSetBottom(clone(rectPrototype), rectBottom(screenFrame))

	rects.bottom_right = clone(rectPrototype)
	rectSetBottom(rects.bottom_right, rectBottom(screenFrame))
	rectSetRight(rects.bottom_right, rectRight(screenFrame))

	-- Edges
	-- These are supposed to snap the window to half screen sizes
	local sideEdgePrototype = { x = screenFrame.x, y = screenFrame.y, w = ann_size, h = screenFrame.h * 0.60 }
	rects.left_edge = rectSetCenterY(clone(sideEdgePrototype), rectCenterY(screenFrame))

	rects.right_edge = rectSetRight(clone(rects.left_edge), rectRight(screenFrame))

	rects.top_edge = rectSetCenterX(
		{ x = screenFrame.x, y = screenFrame.y, w = screenFrame.w * 0.75, h = ann_size },
		rectCenterX(screenFrame)
	)

	local sideSmallEdgePrototype =
		{ x = screenFrame.x, y = screenFrame.y, w = ann_size, h = (rects.left_edge.y - screenFrame.y) * 0.75 }

	rects.left_upper_edge = rectSetCenterY(
		clone(sideSmallEdgePrototype),
		rectBottom(rects.top_left) + (rects.left_edge.y - rectBottom(rects.top_left)) / 2
	)

	rects.left_lower_edge = rectSetCenterY(
		clone(sideSmallEdgePrototype),
		rectBottom(rects.left_edge) + (rects.bottom_left.y - rectBottom(rects.left_edge)) / 2
	)

	rects.right_upper_edge = rectSetRight(clone(rects.left_upper_edge), rectRight(screenFrame))

	rects.right_lower_edge = rectSetRight(clone(rects.left_lower_edge), rectRight(screenFrame))

	return rects
end

local memoize = require("memoize")

-- Memoize generateSnapHotspots so we:
-- 1. Don't have to regenerate the rects over and over again
-- 2. Can compare rects using the == operator
local getSnapHotspots = memoize(generateSnapHotspots)
hs.hotkey.bind({ "cmd", "alt" }, "left", function()
	local rects = getSnapHotspots()

	for idx, rect in pairs(rects) do
		flashRect(rect)
	end
end)

-- local function reverse(tbl)
--	 for i=1, math.floor(#tbl / 2) do
--		 tbl[i], tbl[#tbl - i + 1] = tbl[#tbl - i + 1], tbl[i]
--	 end

--	 return tbl
-- end

-- local function getWindowUnderMouse()
--	-- Invoke `hs.application` because `hs.window.orderedWindows()` doesn't do it
--	-- and breaks itself
--	local _ = hs.application

--	local pos = hs.geometry.new(hs.mouse.getAbsolutePosition())
--	local screen = hs.mouse.getCurrentScreen()

--	 return hs.fnutils.find(hs.window.allWindows(), function(w)
--					return screen == w:screen() and pos:inside(w:frame())
--	 end)
-- end

-- local function printWindows(windows)
--	 print("vvvvv")
--	 for idx, win in ipairs(windows) do

--		 local name = win:application():name()
--		 if name == nil then
--	 name = ""
--		 end

--		 -- print(name)
--		 -- hs.printf("%i %i	 %s", idx, win:id(), name)
--		 hs.printf("%s	%s", hs.inspect(win:id()), name)
--	 end
--	 print("^^^^^")
-- end

-- if mouseTap ~= nil then
--	 mouseTap:stop()
-- end

-- local drag_last_win_id = nil
-- local drag_last_win_frame = nil
-- local drag_last_process_time = 0
-- local mouseTap = hs.eventtap.new({hs.eventtap.event.types.leftMouseDragged}, function(event)

--		 -- Rate limit processing
--		 local cur_time = hs.timer.secondsSinceEpoch()
--		 if cur_time - drag_last_process_time < 0.5 then
--	 return false
--		 end
--		 drag_last_process_time = cur_time

--		 -- print("can run")

--		 -- Which window is the mouse over?
--		 -- local event_window = getWindowUnderMouse()
--		 local event_window = hs.window.focusedWindow()

--		 -- If the mouse isn't over any window, don't do anything...
--		 if not event_window then
--	 return false
--		 end

--		 -- printWindows(hs.window.allWindows())

--		 -- local win_id = event_window:id()
--		 -- local win_frame = event_window:frame()

--		 print(event_window:application():name())
--		 print("has window:", win_id)
--		 print("last win:", drag_last_win_id)

--		 -- -- If the window is being dragged, then
--		 -- -- 1. The mouse needs to be over the window
--		 -- -- 2. The window is being moved
--		 -- if win_id == drag_last_win_id and
--		 --	win_frame.x ~= drag_last_win_frame.x and
--		 --	win_frame.y ~= drag_last_win_frame.y
--		 -- then
--		 --	hs.printf("window %i moved!", win_id)
--		 -- end

--		 -- Record which window the drag event occurred over
--		 drag_last_win_id = win_id
--		 drag_last_win_frame = win_frame

--		 return false
-- end)

-- mouseTap:start()

local printf = hs.printf
local inspect = hs.inspect
local printObj = function(obj)
	printf(inspect(obj))
end

-- local function toggleApp(appName)
--	local app = hs.application.get(appName)
--	printObj(app)
--	if app == nil or not app:isFrontmost() then
--		ok = hs.application.launchOrFocus(appName)
--		if not ok then
--			printf("Could not launch application: ", appName)
--		end
--	else
--		app:hide()
--	end
-- end

local function toggleApp(appName)
	local activeApp = hs.application.frontmostApplication()
	if activeApp:name() ~= appName then
		ok = hs.application.launchOrFocus(appName)
		if not ok then
			printf("Could not launch application: %s", appName)
		end
	else
		activeApp:hide()
	end
end

hs.hotkey.bind({ "cmd" }, "`", function()
	toggleApp("WezTerm")
end)

hs.hotkey.bind({ "alt" }, "space", function()
	toggleApp("ChatGPT")
end)

local spellcheckHotkey = hs.hotkey.bind({ "ctrl", "alt" }, "S", function()
	hs.eventtap.keyStroke({ "cmd" }, "c") -- copy selected word
	hs.timer.doAfter(0.2, function()
		local original = hs.pasteboard.getContents()
		if not original or original == "" then
			return
		end

		print("The word to correct is:", original)

		-- Run spellcheck Python script
		local cmd = string.format("/opt/homebrew/bin/uv run --script ~/.config/spellcheck/spellcheck.py %s", original)

		local handle = io.popen(cmd)
		local result = handle:read("*a")
		handle:close()

		local suggestions = {}
		for word in string.gmatch(result, "([^;]+)") do
			table.insert(suggestions, word)
		end

		if #suggestions == 0 then
			hs.alert.show("No suggestions found")
			return
		end

		-- Show chooser to pick correction
		local chooser = hs.chooser.new(function(choice)
			if not choice then
				return
			end
			hs.pasteboard.setContents(choice.text)
			hs.eventtap.keyStroke({ "cmd" }, "v")
		end)

		local choices = {}
		for _, word in ipairs(suggestions) do
			table.insert(choices, { text = trim(word) })
		end

		chooser:choices(choices)
		chooser:show()
	end)
end)
