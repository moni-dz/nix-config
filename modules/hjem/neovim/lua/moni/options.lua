vim.g.mapleader = " "

local o = vim.o
o.number = true
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
o.winborder = "rounded"

o.mouse = "a"
o.mousemodel = "extend"
o.mousescroll = "ver:3,hor:6"

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

o.wildoptions = "pum,fuzzy"
