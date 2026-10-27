vim.g.mapleader = " "

local o = vim.o
o.number = false
o.signcolumn = "yes"
o.cursorline = true
o.scrolloff = 8
o.splitright = true
o.splitbelow = true
o.ignorecase = true
o.smartcase = true
o.undofile = true
o.expandtab = true
o.shiftwidth = 2
o.tabstop = 2
o.updatetime = 250
o.clipboard = "unnamedplus"

-- copy through the terminal with OSC 52, which also works over ssh. pasting would query the
-- terminal on every `p`, so it reads the unnamed register instead.
do
  local osc52 = require("vim.ui.clipboard.osc52")

  local function paste()
    return { vim.fn.split(vim.fn.getreg(""), "\n"), vim.fn.getregtype("") }
  end

  vim.g.clipboard = {
    name = "OSC 52",
    copy = { ["+"] = osc52.copy("+"), ["*"] = osc52.copy("*") },
    paste = { ["+"] = paste, ["*"] = paste },
  }
end

o.winborder = "rounded"

o.mouse = "a"
o.mousemodel = "extend"
o.mousescroll = "ver:3,hor:6"

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

o.wildoptions = "pum,fuzzy"

vim.keymap.set("n", "<leader>N", function()
  vim.wo.number = not vim.wo.number
end, { desc = "toggle line numbers" })
