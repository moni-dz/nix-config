local icon = {
  [0xF0F6] = "□", [0xF0214] = "□",
  [0xF024B] = "►", [0xF0DCF] = "▼",
  [0xF06D4] = "↔",
  [0xEA85] = "›",
  [0xF01A7] = "ƒ", [0xF0295] = "ƒ",
  [0xF0317] = "¤", [0xF03D7] = "■", [0xF0831] = "◊",
  [0xF01BC] = "·", [0xF0558] = "Σ",
  [0xF002B] = "●", [0xF03FF] = "π", [0xF002C] = "“",
  [0xF03A0] = "#", [0xF0A19] = "?", [0xF016A] = "≡",
  [0xF0169] = "○", [0xF030B] = "κ", [0xF07E2] = "Ø",
  [0xF0645] = "Δ", [0xF140B] = "♪", [0xF0195] = "±",
  [0xF0B1B] = "τ", [0xF0627] = "§",
  [0xF057] = "■", [0xF071] = "▲", [0xF0335] = "•", [0xF05A] = "●",
  [0xF0349] = "≈", [0xF1276] = "~", [0xF0336] = "☼", [0xF02A2] = "\u{E0A0}",
  [0xF02FD] = "●", [0xF017E] = "■", [0xF002A] = "▲", [0xF0CE6] = "◘",
  [0xF0135] = "■", [0xF0131] = "□", [0xF02E9] = "□",
}

local plain = { ["✗"] = "×", ["◦"] = "○", ["▪"] = "■", ["▫"] = "□", ["▾"] = "▼", ["▸"] = "►" }

local function swap(glyph, max)
  local code = vim.fn.char2nr(glyph)
  if code > max then
    return glyph
  end
  return icon[code] or "•"
end

for _, path in ipairs(_G.arg) do
  local file = assert(io.open(path, "rb"))
  local text = file:read("*a")
  file:close()

  local out = text:gsub("[\238\239][\128-\191][\128-\191]", function(g)
    return swap(g, 0xF8FF)
  end)
  out = out:gsub("\243[\176-\191][\128-\191][\128-\191]", function(g)
    return swap(g, 0xFFFFD)
  end)
  out = out:gsub("⏎", "¶"):gsub("↲", "¶")
  for from, to in pairs(plain) do
    out = out:gsub(from, to)
  end

  if out ~= text then
    file = assert(io.open(path, "wb"))
    file:write(out)
    file:close()
  end
end
