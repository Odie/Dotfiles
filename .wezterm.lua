local wezterm = require("wezterm")

local keybinds = {
	{
		key = "+",
		mods = "CMD|SHIFT",
		action = wezterm.action.IncreaseFontSize,
	},
	{
		key = "-",
		mods = "CMD|SHIFT",
		action = wezterm.action.DecreaseFontSize,
	},
}

for i = 1, 8 do
	-- CTRL+ALT + number to move to that position
	table.insert(keybinds, {
		key = tostring(i),
		mods = "CTRL|ALT",
		action = wezterm.action.MoveTab(i - 1),
	})
end

return {
	initial_rows = 100,
	initial_cols = 1000,
	-- window_decorations = "NONE",
	font_size = 14,
	color_scheme = "Gruvbox dark, medium (base16)",
	keys = keybinds,
	window_decorations = "RESIZE",
}
