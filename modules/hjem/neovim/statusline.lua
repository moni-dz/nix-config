-- Statusline with heirline, plain text only and colours follow the active colorscheme.
local heirline = require("heirline")
local conditions = require("heirline.conditions")
local utils = require("heirline.utils")

local function colors()
  return {
    base_bg = utils.get_highlight("Normal").bg,
    bar_bg = utils.get_highlight("StatusLine").bg,
    bar_fg = utils.get_highlight("StatusLine").fg,
    dim = utils.get_highlight("Comment").fg,
    func = utils.get_highlight("Function").fg,
    str = utils.get_highlight("String").fg,
    stmt = utils.get_highlight("Statement").fg,
    const = utils.get_highlight("Constant").fg,
    type = utils.get_highlight("Type").fg,
    err = utils.get_highlight("DiagnosticError").fg,
    warn = utils.get_highlight("DiagnosticWarn").fg,
  }
end

local Mode = {
  init = function(self)
    self.mode = vim.fn.mode(1):sub(1, 1)
  end,
  static = {
    names = {
      n = "NOR", i = "INS", v = "VIS", V = "V-L", ["\22"] = "V-B",
      s = "SEL", S = "S-L", ["\19"] = "S-B", R = "REP", c = "CMD",
      r = "PRM", ["!"] = "SH", t = "TRM", no = "OP",
    },
    colors = {
      n = "func", i = "str", v = "stmt", V = "stmt", ["\22"] = "stmt", R = "const",
      c = "type", t = "str",
    },
  },
  provider = function(self)
    return " " .. (self.names[self.mode] or self.mode) .. " "
  end,
  hl = function(self)
    return { fg = "base_bg", bg = self.colors[self.mode] or "func", bold = true }
  end,
  update = {
    "ModeChanged",
    pattern = "*:*",
    callback = vim.schedule_wrap(function()
      vim.cmd.redrawstatus()
    end),
  },
}

local File = {
  provider = function()
    local name = vim.fn.expand("%:~:.")
    if name == "" then
      name = "[No Name]"
    end
    if vim.bo.modified then
      name = name .. " [+]"
    end
    if not vim.bo.modifiable or vim.bo.readonly then
      name = name .. " [RO]"
    end
    return " " .. name .. " "
  end,
}

-- branch lookup is async and cached per buffer, so redrawing never waits on git
local Branch = {
  condition = function()
    return vim.b.branch ~= nil
  end,
  provider = function()
    return vim.b.branch .. " "
  end,
  hl = { fg = "dim" },
  update = { "BufEnter", "User", pattern = "BranchUpdated" },
}

local Diagnostics = {
  condition = conditions.has_diagnostics,
  init = function(self)
    self.errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
    self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
  end,
  update = { "DiagnosticChanged", "BufEnter" },
  {
    condition = function(self)
      return self.errors > 0
    end,
    provider = function(self)
      return "E" .. self.errors .. " "
    end,
    hl = { fg = "err" },
  },
  {
    condition = function(self)
      return self.warnings > 0
    end,
    provider = function(self)
      return "W" .. self.warnings .. " "
    end,
    hl = { fg = "warn" },
  },
}

local Lsp = {
  condition = conditions.lsp_attached,
  update = { "LspAttach", "LspDetach", "BufEnter" },
  provider = function()
    local names = {}
    for _, c in ipairs(vim.lsp.get_clients({ bufnr = 0 })) do
      table.insert(names, c.name)
    end
    return "[" .. table.concat(names, ",") .. "] "
  end,
  hl = { fg = "dim" },
}

local Ruler = { provider = "%l:%c " }

-- line count and size in bytes, like `(35l 873b)`
local Size = {
  provider = function()
    local last = vim.fn.line("$")
    return string.format(" (%dl %db) ", last, math.max(vim.fn.line2byte(last + 1) - 1, 0))
  end,
  hl = { fg = "dim" },
}

heirline.load_colors(colors())
heirline.setup({
  statusline = {
    hl = { fg = "bar_fg", bg = "bar_bg" },
    Mode,
    File,
    Branch,
    { provider = "%=" },
    Diagnostics,
    Lsp,
    Ruler,
    Size,
  },
})

vim.o.laststatus = 3
vim.o.showmode = false
vim.o.ruler = false

vim.api.nvim_create_autocmd("ColorScheme", {
  callback = function()
    utils.on_colorscheme(colors)
  end,
})

vim.api.nvim_create_autocmd({ "BufEnter", "FocusGained" }, {
  callback = function(ev)
    local dir = vim.fn.expand("%:p:h")
    if vim.bo[ev.buf].buftype ~= "" or dir == "" then
      return
    end
    -- jj repos show the change id and bookmarks of @, anything else the git branch
    local root = vim.fs.root(dir, { ".jj", ".git" })
    local cmd = { "git", "-C", dir, "branch", "--show-current" }
    if root and vim.uv.fs_stat(root .. "/.jj") then
      cmd = {
        "jj", "log", "-R", root, "-r", "@", "--no-graph", "--ignore-working-copy", "--color", "never",
        "-T", [[change_id.shortest(4) ++ " " ++ bookmarks]],
      }
    end
    vim.system(cmd, { text = true }, function(res)
      vim.schedule(function()
        if vim.api.nvim_buf_is_valid(ev.buf) then
          local b = vim.trim(res.stdout or "")
          vim.b[ev.buf].branch = b ~= "" and b or nil
          vim.api.nvim_exec_autocmds("User", { pattern = "BranchUpdated" })
          vim.cmd.redrawstatus()
        end
      end)
    end)
  end,
})
