local M = {}

local function to_number(hex)
  return hex and tonumber(hex:gsub("^#", ""), 16) or nil
end

function M.palette(name)
  local ok, hex = pcall(function()
    return require("mellifluous.colors").get_colors()[name].hex
  end)
  return ok and to_number(hex) or nil
end

function M.theme_bg()
  local ok, hex = pcall(function()
    local config = require("mellifluous.config").config
    local set = require("mellifluous.colors.colorsets." .. config.colorset)
    return (config.is_bg_dark and set.get_bg_dark() or set.get_bg_light()).hex
  end)
  return ok and to_number(hex) or nil
end

function M.mix(fg, bg, alpha)
  local function channel(shift)
    local f, b = bit.band(bit.rshift(fg, shift), 0xff), bit.band(bit.rshift(bg, shift), 0xff)
    return math.floor(b + (f - b) * alpha + 0.5)
  end
  return channel(16) * 0x10000 + channel(8) * 0x100 + channel(0)
end

return M
