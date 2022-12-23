-- print-elapsed.lu
--
-- This file takes in a parameter which is supposed to indicate the time elapsed for the
-- last commandline command.
--
-- We want something that starts at some color and slides towards red the longer it
-- took for the command to run.

-- ESC Code Sequence	Description
-- ESC[38;2;{r};{g};{b}m	Set foreground color as RGB.
-- ESC[48;2;{r};{g};{b}m	Set background color as RGB.

-- Returns an escape code that changes the foreground to the indicated color
local function fg(r, g, b)
  return string.format("\27[38;2;%d;%d;%d;255m", r, g, b)
end

function hslToRgb(h, s, l)
    h = h / 360
    s = s / 100
    l = l / 100

    local r, g, b;

    if s == 0 then
        r, g, b = l, l, l; -- achromatic
    else
        local function hue2rgb(p, q, t)
            if t < 0 then t = t + 1 end
            if t > 1 then t = t - 1 end
            if t < 1 / 6 then return p + (q - p) * 6 * t end
            if t < 1 / 2 then return q end
            if t < 2 / 3 then return p + (q - p) * (2 / 3 - t) * 6 end
            return p;
        end

        local q = l < 0.5 and l * (1 + s) or l + s - l * s;
        local p = 2 * l - q;
        r = hue2rgb(p, q, h + 1 / 3);
        g = hue2rgb(p, q, h);
        b = hue2rgb(p, q, h - 1 / 3);
    end

    if not a then a = 1 end
    return r * 255, g * 255, b * 255, a * 255
end

local function interp(startVal, endVal, percent)
  return startVal + (endVal - startVal) * percent
end

local function clamp(x, min, max)
    return math.max(math.min(x, max), min)
end

local function clampBetween(x, v1, v2)
  if v1 > v2 then
    return clamp(x, v2, v1)
  else
    return clamp(x, v1, v2)
  end
end

local function clampedReverseInterp(startVal, endVal, val)
  val = clampBetween(val, startVal, endVal)
  return (val-startVal) / (endVal-startVal)
end

local function interpColor(startColor, endColor, percent)
  return {
    interp(startColor[1], endColor[1], percent),
    interp(startColor[2], endColor[2], percent),
    interp(startColor[3], endColor[3], percent)
  }
end

local reset_fgbg = "\27[39;49m"
local reset_fg = "\27[49m"
local reset_color = "\27[0m"

local startHSL = {116, 51, 55}
local endHSL = {0, 51, 55}

function colorPercentFromElapsed(elapsed)
  return clampedReverseInterp(5, 60, elapsed)
end

-- How long did it to run the command?
local elapsed = tonumber(arg[1])

-- How far along the color slider do we want to move?
local percent = colorPercentFromElapsed(elapsed)

-- What's the actual color we want to show?
local colorHSL = interpColor(startHSL, endHSL, percent)
local r,g,b = hslToRgb(unpack(colorHSL))

-- Print out the colorized elapsed time
print(string.format("Elapsed: %s%.03f%s s", fg(r, g, b), elapsed, reset_color))
