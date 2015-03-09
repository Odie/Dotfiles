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
