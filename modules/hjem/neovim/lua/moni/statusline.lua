local heirline = require("heirline")
local conditions = require("heirline.conditions")
local utils = require("heirline.utils")
local color = require("moni.colors")

local function colors()
  return {
    badge_fg = color.theme_bg() or color.palette("bg"),
    num = color.palette("ui_yellow"),
    base_bg = utils.get_highlight("Normal").bg or color.theme_bg(),
    base_fg = utils.get_highlight("Normal").fg,
    tab_bg = color.palette("bg4"),
    bar_fg = utils.get_highlight("StatusLine").fg,
    chip_err = color.palette("red"),
    chip_warn = color.palette("orange"),
    -- mellifluous' own green is olive, this is the green of the Rex theme files next to it
    chip_ok = vim.o.background == "dark" and 0x98bc98 or 0x23702b,
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
    return { fg = "badge_fg", bg = self.colors[self.mode] or "func", bold = true }
  end,
  update = {
    "ModeChanged",
    pattern = "*:*",
    callback = vim.schedule_wrap(function()
      vim.cmd.redrawstatus()
    end),
  },
}

local SearchCount = {
  condition = function()
    return vim.v.hlsearch == 1
  end,
  provider = function()
    local ok, count = pcall(vim.fn.searchcount, { maxcount = 999 })
    if not ok or not count.total or count.total == 0 then
      return ""
    end
    return string.format("[%d/%d%s] ", count.current, count.total, count.incomplete > 0 and "+" or "")
  end,
  hl = { fg = "num" },
}

local Branch = {
  condition = function()
    return vim.b.branch ~= nil
  end,
  provider = function()
    return " " .. vim.fn.nr2char(0xe0a0) .. " " .. vim.b.branch .. " "
  end,
  hl = { fg = "badge_fg", bg = "stmt", bold = true },
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
      return " ■ " .. self.errors .. " "
    end,
    hl = { fg = "badge_fg", bg = "chip_err", bold = true },
  },
  {
    condition = function(self)
      return self.errors > 0
    end,
    provider = " ",
  },
  {
    condition = function(self)
      return self.warnings > 0
    end,
    provider = function(self)
      return " ▲ " .. self.warnings .. " "
    end,
    hl = { fg = "badge_fg", bg = "chip_warn", bold = true },
  },
  {
    condition = function(self)
      return self.warnings > 0
    end,
    provider = " ",
  },
}

local lsp_names, lsp_failed = {}, {}

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client then
      lsp_names[client.id] = client.name
      lsp_failed[client.name] = nil
    end
  end,
})

vim.lsp.config("*", {
  on_exit = function(code, signal, client_id)
    local name = lsp_names[client_id]
    if name and (code ~= 0 or signal ~= 0) then
      lsp_failed[name] = true
      vim.schedule(function()
        vim.api.nvim_exec_autocmds("User", { pattern = "MoniLspFailed" })
      end)
    end
  end,
})

local function failed_here()
  local names = {}
  for name in pairs(lsp_failed) do
    local filetypes = (vim.lsp.config[name] or {}).filetypes or {}
    if vim.tbl_contains(filetypes, vim.bo.filetype) then
      table.insert(names, name)
    end
  end
  table.sort(names)
  return names
end

local Lsp = {
  condition = function()
    return conditions.lsp_attached() or #failed_here() > 0
  end,
  update = { "LspAttach", "LspDetach", "BufEnter", "User", pattern = "MoniLspFailed" },
  init = function(self)
    self.failed = failed_here()
    self.names = {}
    for _, c in ipairs(vim.lsp.get_clients({ bufnr = 0 })) do
      table.insert(self.names, c.name)
    end
    vim.list_extend(self.names, self.failed)
  end,
  provider = function(self)
    return " ♦ " .. table.concat(self.names, ",") .. " "
  end,
  hl = function(self)
    return { fg = "badge_fg", bg = #self.failed > 0 and "chip_err" or "chip_ok", bold = true }
  end,
}

local LspGap = { condition = Lsp.condition, provider = " " }

local Ruler = {
  provider = function()
    local pos = vim.api.nvim_win_get_cursor(0)
    return string.format(" %s %d:%d ", vim.fn.nr2char(0xe0a1), pos[1], pos[2] + 1)
  end,
  hl = { fg = "num" },
}

local Size = {
  provider = function()
    local last = vim.fn.line("$")
    return string.format(" (%dl %db) ", last, math.max(vim.fn.line2byte(last + 1) - 1, 0))
  end,
  hl = { fg = "dim" },
}

local TabFile = {
  provider = function(self)
    local name = vim.fn.fnamemodify(self.filename, ":t")
    return " " .. (name == "" and "[No Name]" or name) .. (self.modified and " ●" or "")
  end,
  on_click = {
    callback = function(_, minwid, _, button)
      if button == "m" then
        pcall(Snacks.bufdelete, minwid)
      elseif button == "r" then
        require("moni.contextmenu").tab(minwid)
      else
        for _, win in ipairs({ vim.api.nvim_get_current_win(), unpack(vim.api.nvim_tabpage_list_wins(0)) }) do
          if vim.bo[vim.api.nvim_win_get_buf(win)].buftype == "" and vim.api.nvim_win_get_config(win).relative == "" then
            vim.api.nvim_set_current_win(win)
            break
          end
        end
        vim.cmd.buffer(minwid)
      end
    end,
    minwid = function(self)
      return self.bufnr
    end,
    name = "heirline_tabline_buffer",
  },
}

local TabClose = {
  provider = " × ",
  hl = { fg = "dim" },
  on_click = {
    callback = function(_, minwid)
      local ok, err = pcall(Snacks.bufdelete, minwid)
      if not ok then
        vim.notify(tostring(err), vim.log.levels.WARN)
      end
    end,
    minwid = function(self)
      return self.bufnr
    end,
    name = "heirline_tabline_close",
  },
}

local Tab = {
  init = function(self)
    self.filename = vim.api.nvim_buf_get_name(self.bufnr)
    self.modified = vim.bo[self.bufnr].modified
  end,
  hl = function(self)
    if self.is_active then
      return { fg = "base_fg", bg = "base_bg", bold = true }
    end
    return { fg = "dim", bg = "tab_bg" }
  end,
  TabFile,
  TabClose,
}

local TabLine = {
  utils.make_buflist(Tab, { provider = " ◄ ", hl = { fg = "dim" } }, { provider = " ► ", hl = { fg = "dim" } }),
  { provider = "%=" },
  hl = { bg = "tab_bg" },
}

heirline.load_colors(colors())
heirline.setup({
  statusline = {
    hl = { fg = "bar_fg" },
    Mode,
    SearchCount,
    { provider = "%=" },
    Ruler,
    Size,
    Diagnostics,
    Lsp,
    LspGap,
    Branch,
  },
  tabline = TabLine,
})

vim.o.laststatus = 3
vim.o.showmode = false
vim.o.ruler = false
vim.o.cmdheight = 0
vim.o.showcmd = false

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
