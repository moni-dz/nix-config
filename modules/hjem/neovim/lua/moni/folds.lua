local o = vim.o
o.foldcolumn = "0"
o.foldlevel = 99
o.foldlevelstart = 99
o.foldenable = true
o.fillchars = "fold: ,foldsep: ,eob: "

function _G.fold_click()
  local pos = vim.fn.getmousepos()
  local cmd = vim.fn.foldclosed(pos.line) == -1 and "foldclose" or "foldopen"
  vim.api.nvim_win_call(pos.winid, function()
    pcall(vim.cmd, pos.line .. cmd)
  end)
end

function _G.fold_mark()
  local lnum = vim.v.lnum
  if vim.v.virtnum ~= 0 then
    return " "
  end
  if vim.fn.foldclosed(lnum) == lnum then
    return "%@v:lua.fold_click@%#FoldMark#►%T"
  end
  if vim.fn.foldlevel(lnum) > vim.fn.foldlevel(lnum - 1) then
    return "%@v:lua.fold_click@%#FoldMark#▼%T"
  end
  return " "
end

o.statuscolumn = "%s%=%l %{%v:lua.fold_mark()%} "

require("ufo").setup({
  provider_selector = function()
    return { "treesitter", "indent" }
  end,
})
vim.keymap.set("n", "zR", require("ufo").openAllFolds, { desc = "open all folds" })
vim.keymap.set("n", "zM", require("ufo").closeAllFolds, { desc = "close all folds" })
