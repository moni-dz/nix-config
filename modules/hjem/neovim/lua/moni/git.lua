require("gitsigns").setup({
  base = "HEAD",
  signs = {
    add = { text = "▎" },
    change = { text = "▎" },
    delete = { text = "▁" },
    topdelete = { text = "▔" },
    changedelete = { text = "▎" },
    untracked = { text = "▎" },
  },
  linehl = true,
  on_attach = function(buf)
    local gs = require("gitsigns")
    local map = function(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { buffer = buf, desc = desc })
    end
    map("]h", function() gs.nav_hunk("next") end, "next hunk")
    map("[h", function() gs.nav_hunk("prev") end, "previous hunk")
    map("<leader>hp", gs.preview_hunk, "preview hunk")
    map("<leader>hr", gs.reset_hunk, "reset hunk")
    map("<leader>hd", gs.diffthis, "compare with the parent")
    map("<leader>hb", function() gs.blame_line({ full = true }) end, "blame line")
  end,
})

require("hunk").setup({
  icons = {
    enable_file_icons = false,
    selected = "●",
    deselected = "○",
    partially_selected = "◘",
    folder_open = "▼",
    folder_closed = "►",
    expanded = "▼",
    collapsed = "►",
  },
})
