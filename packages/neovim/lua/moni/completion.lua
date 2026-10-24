require("blink.cmp").setup({
  appearance = {
    kind_icons = {
      Text = "≡", Method = "ƒ", Function = "ƒ", Constructor = "⌂", Field = "·", Variable = "●",
      Class = "◊", Interface = "○", Module = "□", Property = "·", Unit = "µ", Value = "#",
      Enum = "Σ", Keyword = "κ", Snippet = "§", Color = "☼", File = "▬", Reference = "→",
      Folder = "►", EnumMember = "ε", Constant = "π", Struct = "Δ", Event = "♪", Operator = "±",
      TypeParameter = "τ", Command = "▸", Subcommand = "›", Option = "-", Path = "▬",
    },
  },
  sources = {
    per_filetype = { ["moni-run"] = { "shell" } },
    providers = {
      shell = { name = "Shell", module = "moni.shellsource" },
      -- :Run lines are the shell source's, the stock one would only offer vim's own completion for them
      cmdline = {
        enabled = function()
          return not vim.fn.getcmdline():match("^%s*Run%s")
        end,
      },
    },
  },
  cmdline = {
    sources = function()
      if vim.fn.getcmdtype() == ":" then
        return { "cmdline", "shell" }
      end
      return vim.fn.getcmdtype() == "@" and { "cmdline" } or { "buffer" }
    end,
  },
  keymap = {
    preset = "default",
    ["<Tab>"] = { "select_and_accept", "snippet_forward", "fallback" },
    ["<S-Tab>"] = { "snippet_backward", "fallback" },
    ["<CR>"] = { "accept", "fallback" },
  },
  completion = {
    list = { selection = { preselect = false, auto_insert = false } },
    menu = {
      cmdline_position = function()
        return require("moni.cmdline").position()
      end,
      min_width = 24,
      border = "rounded",
      scrollbar = false,
      draw = {
        columns = { { "kind_icon" }, { "label" }, { "kind" } },
        components = { label = { width = { fill = true, max = 36 } } },
      },
    },
    documentation = {
      auto_show = true,
      auto_show_delay_ms = 300,
      window = { min_width = 30, max_width = 60, max_height = 16, border = "rounded", scrollbar = false },
    },
  },
  signature = { enabled = true },
  fuzzy = { implementation = "prefer_rust" },
})

local function over_blink_menu()
  local ok, menu = pcall(require, "blink.cmp.completion.windows.menu")
  if not ok or not menu.win:is_open() then
    return nil
  end
  local pos = vim.fn.getmousepos()
  if pos.winid == menu.win:get_win() then
    return pos
  end
end

for _, mode in ipairs({ "i", "c" }) do
  vim.keymap.set(mode, "<LeftMouse>", function()
    local pos = over_blink_menu()
    if not pos then
      return "<LeftMouse>"
    end
    vim.schedule(function()
      require("blink.cmp.completion.list").accept({ index = pos.line })
    end)
    return ""
  end, { expr = true, desc = "click completion item" })

  for key, step in pairs({ ["<ScrollWheelDown>"] = "select_next", ["<ScrollWheelUp>"] = "select_prev" }) do
    vim.keymap.set(mode, key, function()
      if not over_blink_menu() then
        return key
      end
      vim.schedule(function()
        require("blink.cmp.completion.list")[step]({ count = 3 })
      end)
      return ""
    end, { expr = true, desc = "scroll completion menu" })
  end
end
