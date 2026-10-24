local color = require("moni.colors")
local set = vim.api.nvim_set_hl

local function number_colors()
  local fg, bg, tan = color.palette("fg"), color.theme_bg(), color.palette("yellow")
  if not (fg and bg and tan) then
    return
  end
  local dim = color.mix(fg, bg, 0.4)
  for name, fg in pairs({ LineNr = dim, CursorLineNr = tan }) do
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    set(0, name, vim.tbl_extend("force", hl, { fg = fg, bold = name == "CursorLineNr" }))
  end
end
number_colors()
vim.api.nvim_create_autocmd("ColorScheme", { callback = number_colors })

local function flat_gutter()
  local names = { "LineNr", "SignColumn", "FoldColumn" }
  for _, kind in ipairs({ "Add", "Change", "Delete", "Topdelete", "Changedelete", "Untracked" }) do
    table.insert(names, "GitSigns" .. kind)
  end
  for _, kind in ipairs({ "Error", "Warn", "Info", "Hint", "Ok" }) do
    table.insert(names, "DiagnosticSign" .. kind)
  end
  for _, name in ipairs(names) do
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    if hl.bg then
      hl.bg = nil
      set(0, name, hl)
    end
  end
end
vim.api.nvim_create_autocmd("ColorScheme", {
  callback = function()
    vim.schedule(flat_gutter)
  end,
})

local function git_line_tints()
  local function hl_color(name, attr)
    return vim.api.nvim_get_hl(0, { name = name, link = false })[attr]
  end
  local bg = hl_color("Normal", "bg") or color.palette("bg") or (vim.o.background == "dark" and 0x000000 or 0xffffff)
  for line, sign in pairs({
    GitSignsAddLn = "GitSignsAdd",
    GitSignsChangeLn = "GitSignsChange",
    GitSignsDeleteLn = "GitSignsDelete",
  }) do
    local fg = hl_color(sign, "fg")
    if fg then
      set(0, line, { bg = color.mix(fg, bg, 0.12) })
    end
  end
end
git_line_tints()
vim.api.nvim_create_autocmd("ColorScheme", { callback = git_line_tints })
flat_gutter()

local function quiet_colors()
  local fg, dim, tan, bg = color.palette("fg"), color.palette("comments"), color.palette("yellow"), color.theme_bg()
  if not (fg and dim and tan and bg) then
    return
  end
  local mix = color.mix
  set(0, "SnacksIndent", { fg = mix(fg, bg, 0.08) })
  set(0, "SnacksIndentScope", { fg = mix(dim, bg, 0.7) })
  for _, name in ipairs({ "LspReferenceText", "LspReferenceRead", "LspReferenceWrite" }) do
    set(0, name, { bg = mix(fg, bg, 0.1) })
  end
  set(0, "LspInlayHint", { fg = mix(fg, bg, 0.38), italic = true })
  set(0, "NormalFloat", { bg = bg })
  set(0, "Pmenu", { bg = bg })
  set(0, "FloatBorder", { fg = mix(fg, bg, 0.3), bg = bg })
  for _, name in ipairs({ "BlinkCmpMenuBorder", "BlinkCmpDocBorder", "BlinkCmpSignatureHelpBorder" }) do
    set(0, name, { link = "FloatBorder" })
  end
  for _, name in ipairs({ "BlinkCmpMenu", "BlinkCmpDoc", "BlinkCmpSignatureHelp", "BlinkCmpScrollBarGutter" }) do
    set(0, name, { bg = bg })
  end
  local selected = mix(fg, bg, 0.14)
  for _, name in ipairs({ "PmenuSel", "BlinkCmpMenuSelection", "BlinkCmpDocCursorLine", "BlinkCmpSignatureHelpActiveParameter" }) do
    set(0, name, { bg = selected })
  end
  set(0, "DropBarMenuHoverEntry", { bg = selected })
  for _, name in ipairs({ "PmenuSbar", "DropBarMenuSbar" }) do
    set(0, name, { bg = bg })
  end
  set(0, "BlinkCmpKind", { fg = mix(fg, bg, 0.45) })
  set(0, "BlinkCmpLabelMatch", { fg = tan, bold = true })
  set(0, "FloatTitle", { fg = mix(fg, bg, 0.55), bg = bg, italic = true })
  set(0, "WinBar", { fg = mix(fg, bg, 0.55) })
  set(0, "WinBarNC", { fg = mix(fg, bg, 0.4) })
  for _, kind in ipairs({ "Error", "Warn", "Info", "Hint" }) do
    local sev = vim.api.nvim_get_hl(0, { name = "Diagnostic" .. kind, link = false }).fg
    if sev then
      set(0, "DiagnosticVirtualText" .. kind, { fg = mix(sev, bg, 0.65) })
      set(0, "DiagnosticVirtualLines" .. kind, { fg = mix(sev, bg, 0.85) })
    end
  end
  local warn = vim.api.nvim_get_hl(0, { name = "DiagnosticWarn", link = false }).fg
  set(0, "JjBlame", { fg = mix(fg, bg, 0.4), italic = true })
  set(0, "JjConflict", { bg = warn and mix(warn, bg, 0.14) or nil })
  set(0, "FlashLabel", { fg = bg, bg = tan, bold = true })
  set(0, "FlashMatch", { fg = fg, bg = mix(fg, bg, 0.22) })
  set(0, "FlashCurrent", { fg = fg, bg = mix(tan, bg, 0.4) })
  set(0, "FlashBackdrop", { fg = mix(fg, bg, 0.35) })
  set(0, "MoniMenuHint", { fg = mix(fg, bg, 0.4) })
  set(0, "MoniMenuSeparator", { fg = mix(fg, bg, 0.2) })
  for _, name in ipairs({ "StatusLine", "StatusLineNC" }) do
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    hl.bg = nil
    set(0, name, hl)
  end
  set(0, "FoldMark", { fg = mix(fg, bg, 0.3) })
  for _, name in ipairs({ "DropBarIconUISeparator", "DropBarIconUIIndicator" }) do
    set(0, name, { fg = mix(fg, bg, 0.4) })
  end
end
quiet_colors()
vim.api.nvim_create_autocmd("ColorScheme", {
  callback = function()
    vim.schedule(quiet_colors)
  end,
})
