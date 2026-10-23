require("flash").setup({
  labels = "asdfghjklqwertyuiopzxcvbnm",
  prompt = { prefix = { { "› ", "FlashPromptIcon" } } },
  modes = { search = { enabled = false }, char = { jump_labels = true } },
})
vim.keymap.set({ "n", "x", "o" }, "s", function()
  require("flash").jump()
end, { desc = "jump to text" })
vim.keymap.set({ "n", "x", "o" }, "S", function()
  require("flash").treesitter()
end, { desc = "select a treesitter node" })
vim.keymap.set("o", "r", function()
  require("flash").remote()
end, { desc = "act on text elsewhere" })
vim.keymap.set({ "o", "x" }, "R", function()
  require("flash").treesitter_search()
end, { desc = "treesitter search" })
vim.keymap.set("c", "<C-s>", function()
  require("flash").toggle()
end, { desc = "flash in the / search" })

local ai = require("mini.ai")
ai.setup({
  n_lines = 300,
  mappings = { around_next = "aN", inside_next = "iN", around_last = "aL", inside_last = "iL" },
  custom_textobjects = {
    f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
    c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }),
    a = ai.gen_spec.treesitter({ a = "@parameter.outer", i = "@parameter.inner" }),
    o = ai.gen_spec.treesitter({
      a = { "@conditional.outer", "@loop.outer" },
      i = { "@conditional.inner", "@loop.inner" },
    }),
  },
})

require("mini.bracketed").setup({ comment = { suffix = "" }, undo = { suffix = "" } })
