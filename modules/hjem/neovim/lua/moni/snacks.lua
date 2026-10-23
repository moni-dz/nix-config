if vim.env.TERM_PROGRAM == "rex" then
  vim.env.SNACKS_KITTY = "1"
end
vim.o.smoothscroll = false
require("snacks").setup({
  notifier = {
    timeout = 3000,
    icons = { error = "■", warn = "▲", info = "●", debug = "•", trace = "·" },
  },
  indent = {
    indent = { char = "▏" },
    scope = { char = "▏" },
    animate = { enabled = false },
  },
  words = { debounce = 150 },
  image = {
    math = { enabled = false },
    icons = { math = "∑ ", chart = "≡ ", image = "□ " },
  },
})
vim.keymap.set("n", "<leader>i", function()
  Snacks.image.hover()
end, { desc = "preview image under cursor" })

vim.keymap.set("n", "<leader>bd", function()
  Snacks.bufdelete()
end, { desc = "close buffer, keep the window" })

vim.keymap.set("n", "]]", function()
  Snacks.words.jump(vim.v.count1, true)
end, { desc = "next reference" })
vim.keymap.set("n", "[[", function()
  Snacks.words.jump(-vim.v.count1, true)
end, { desc = "previous reference" })
