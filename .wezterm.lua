local wezterm = require 'wezterm'

local mykeys = {}
for i = 1, 8 do
  -- CTRL+ALT + number to move to that position
  table.insert(mykeys, {
    key = tostring(i),
    mods = 'CTRL|ALT',
    action = wezterm.action.MoveTab(i - 1),
  })
end

return {
  initial_rows = 100,
  initial_cols = 1000,
  -- window_decorations = "NONE",
  font_size = 14,
  color_scheme = "Gruvbox dark, medium (base16)",
  keys = mykeys,
}
