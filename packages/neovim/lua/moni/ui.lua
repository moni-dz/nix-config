require("which-key").setup({
  preset = "helix",
  icons = {
    separator = "→",
    mappings = false,
    rules = false,
    keys = {
      Up = "↑", Down = "↓", Left = "←", Right = "→",
      C = "C-", M = "M-", D = "D-", S = "S-",
      CR = "CR", Esc = "Esc", NL = "NL", BS = "BS", Space = "Space", Tab = "Tab",
      ScrollWheelDown = "WheelDn", ScrollWheelUp = "WheelUp",
      F1 = "F1", F2 = "F2", F3 = "F3", F4 = "F4", F5 = "F5", F6 = "F6",
      F7 = "F7", F8 = "F8", F9 = "F9", F10 = "F10", F11 = "F11", F12 = "F12",
    },
  },
})
require("which-key").add({
  { "<leader>f", group = "find" },
  { "<leader>h", group = "hunk" },
  { "<leader>b", group = "buffer" },
  { "<leader>j", group = "jj" },
  { "<leader>m", group = "markdown" },
  { "gz", group = "surround" },
  { "<leader>r", group = "run" },
  { "<leader>x", group = "diagnostics" },
})
vim.keymap.set("n", "<leader>?", function()
  require("which-key").show({ global = false })
end, { desc = "buffer-local keymaps" })

require("dropbar").setup({
  icons = {
    enable = false,
    ui = {
      bar = { separator = " › ", extends = "…" },
      menu = { separator = " ", indicator = "► " },
    },
  },
})
