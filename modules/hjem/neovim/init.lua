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

-- mouse: click, drag to select/resize splits, wheel scroll, right click menu, ctrl-click to jump
o.mouse = "a"
o.mousemodel = "popup_setpos"
o.mousescroll = "ver:3,hor:6"
o.smoothscroll = true

-- plain ASCII/box drawing only, no icon glyphs that need a patched font
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_winsize = 25

o.wildoptions = "pum,fuzzy"

require("mellifluous").setup({})
vim.cmd.colorscheme("mellifluous")

vim.api.nvim_create_autocmd("FileType", {
  callback = function(ev)
    pcall(vim.treesitter.start, ev.buf)
  end,
})

-- no nvim-lspconfig, servers come from the wrapper's PATH
vim.lsp.config("nixd", {
  cmd = { "nixd" },
  filetypes = { "nix" },
  root_markers = { "flake.nix", ".git" },
  settings = { nixd = { formatting = { command = { "nixfmt" } } } },
})

vim.lsp.config("tinymist", {
  cmd = { "tinymist" },
  filetypes = { "typst" },
  root_markers = { ".git" },
})

vim.lsp.config("clangd", {
  cmd = { "clangd", "--background-index", "--clang-tidy" },
  filetypes = { "c", "cpp", "objc", "objcpp" },
  root_markers = { "compile_commands.json", ".clangd", ".git" },
})

vim.lsp.config("rust_analyzer", {
  cmd = { "rust-analyzer" },
  filetypes = { "rust" },
  root_markers = { "Cargo.toml", "rust-project.json", ".git" },
})

vim.lsp.enable({ "nixd", "tinymist", "clangd", "rust_analyzer" })

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client and client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
    end
  end,
})

-- jj colocated repos keep a git HEAD at the parent of @, so diffing against HEAD (not the index)
-- shows exactly what `jj diff` shows; plain git repos get the same, minus staging
require("gitsigns").setup({
  base = "HEAD",
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "-" },
    changedelete = { text = "~" },
    untracked = { text = "?" },
  },
  on_attach = function(buf)
    local gs = require("gitsigns")
    local map = function(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { buffer = buf, desc = desc })
    end
    map("]h", function() gs.nav_hunk("next") end, "next hunk")
    map("[h", function() gs.nav_hunk("prev") end, "previous hunk")
    map("<leader>hp", gs.preview_hunk, "preview hunk")
    map("<leader>hr", gs.reset_hunk, "reset hunk")
    map("<leader>hb", function() gs.blame_line({ full = true }) end, "blame line")
  end,
})

-- jj: `:J <subcommand>` runs any jj command, these open the UI integrated ones
require("jj").setup({})
vim.keymap.set("n", "<leader>jl", "<Cmd>J log<CR>", { desc = "jj log" })
vim.keymap.set("n", "<leader>js", "<Cmd>J status<CR>", { desc = "jj status" })
vim.keymap.set("n", "<leader>jd", "<Cmd>J describe<CR>", { desc = "jj describe" })
vim.keymap.set("n", "<leader>jn", "<Cmd>J new<CR>", { desc = "jj new" })
vim.keymap.set("n", "<leader>jf", "<Cmd>J diff<CR>", { desc = "jj diff of this file" })

-- smart-splits: ctrl-hjkl moves across nvim windows and Rex panes, alt-hjkl resizes
-- (Rex must pass ctrl+hjkl through when nvim is foreground, see smart-splits-backend-rex's README)
require("smart-splits-backend-rex").setup({})
require("smart-splits").setup({ mux = { backend = "smart-splits-backend-rex" } })
local ss = require("smart-splits")
vim.keymap.set("n", "<C-h>", ss.move_cursor_left, { desc = "focus left" })
vim.keymap.set("n", "<C-j>", ss.move_cursor_down, { desc = "focus down" })
vim.keymap.set("n", "<C-k>", ss.move_cursor_up, { desc = "focus up" })
vim.keymap.set("n", "<C-l>", ss.move_cursor_right, { desc = "focus right" })
vim.keymap.set("n", "<A-h>", ss.resize_left, { desc = "resize left" })
vim.keymap.set("n", "<A-j>", ss.resize_down, { desc = "resize down" })
vim.keymap.set("n", "<A-k>", ss.resize_up, { desc = "resize up" })
vim.keymap.set("n", "<A-l>", ss.resize_right, { desc = "resize right" })

vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float)
-- fzf-lua: fd/rg/fzf do the work, no icons so any font renders it
require("fzf-lua").setup({
  "default-title",
  files = { file_icons = false, git_icons = false },
  grep = { file_icons = false, git_icons = false },
  buffers = { file_icons = false, git_icons = false },
  fzf_opts = { ["--no-scrollbar"] = true },
})
vim.keymap.set("n", "<leader>ff", "<Cmd>FzfLua files<CR>", { desc = "find file" })
vim.keymap.set("n", "<leader>fg", "<Cmd>FzfLua live_grep<CR>", { desc = "grep" })
vim.keymap.set("n", "<leader>fb", "<Cmd>FzfLua buffers<CR>", { desc = "buffers" })
vim.keymap.set("n", "<leader>fr", "<Cmd>FzfLua oldfiles<CR>", { desc = "recent files" })
vim.keymap.set("n", "<leader>t", "<Cmd>Lexplore<CR>", { desc = "file tree, double click opens" })

-- right click menu, the LSP "Go to definition" and friends are added by Neovim itself
vim.cmd.amenu([[PopUp.-LspSep- <Nop>]])
vim.cmd.amenu([[PopUp.Rename <Cmd>lua vim.lsp.buf.rename()<CR>]])
vim.cmd.amenu([[PopUp.Code\ Action <Cmd>lua vim.lsp.buf.code_action()<CR>]])
vim.cmd.amenu([[PopUp.References <Cmd>lua vim.lsp.buf.references()<CR>]])
vim.cmd.amenu([[PopUp.Format <Cmd>lua vim.lsp.buf.format()<CR>]])
