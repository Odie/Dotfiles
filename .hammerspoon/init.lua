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
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "M", function()
	local focusedWindow = hs.window.focusedWindow()
	local origFrame = maxWindowOrigFrame[focusedWindow:id()]

	if origFrame ~= nil then
		focusedWindow:setFrame(origFrame)
		maxWindowOrigFrame[focusedWindow:id()] = nil
	else
		maxWindowOrigFrame[focusedWindow:id()] = focusedWindow:frame()
		focusedWindow:maximize()
	end
end)

-------------------------------------------------------------------------------
-- Split window right and left
-------------------------------------------------------------------------------

-- Resize the window to half of the screen and algin it to left
-- or right of the screen.
-- The param "rightOrLeft" should either the string "right" or "left"
local function halfScreen(window, rightOrLeft)
	local xMultiple = 0

	-- Input param sanity check
	if rightOrLeft == "right" then
		 xMultiple = 1
	elseif rightOrLeft == "left" then
		 xMultiple = 0
	else
		return
	end

	-- Which screen is the window on?
	local screen = window:screen()

	-- Figure out the new window frame based on screen size
	local fullFrame = screen:frame()
	local newFrame = {}

	newFrame.h = fullFrame.h
	newFrame.y = fullFrame.y

	newFrame.x = xMultiple * (fullFrame.w / 2)
	newFrame.w = fullFrame.w / 2

	-- Set the new frame for the specified window
	window:setFrame(newFrame)
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "left", function()
	local focusedWindow = hs.window.focusedWindow()
	halfScreen(focusedWindow, "left")
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "right", function()
	local focusedWindow = hs.window.focusedWindow()
	halfScreen(focusedWindow, "right")
end)
