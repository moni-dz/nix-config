 table.insert(package.loaders, function(...)
   return require("thyme").loader(...)
 end)

 vim.opt.rtp:prepend(vim.fn.stdpath("cache") .. "/thyme/compiled")
 vim.loader.enable()

 local thyme_config = vim.fs.joinpath(vim.fn.stdpath("config"), ".nvim-thyme.fnl")
 if vim.uv.fs_stat(thyme_config) then
   vim.secure.trust({ action = "allow", path = thyme_config })
 end

 vim.api.nvim_create_autocmd("VimEnter", {
   once = true,
   callback = function()
     vim.schedule(function()
       require("thyme").setup()
     end)
   end,
 })

 require("moni")
-- The only Lua entry point; the rest of the config is Fennel under fnl/, compiled on demand by nvim-thyme.
-- thyme looks for modules in <stdpath("config")>/fnl, which hjem links to the built config; macros come off the rtp.
table.insert(package.loaders, function(...)
  return require("thyme").loader(...)
end)
-- the path must contain "/thyme/compile" for thyme to find its output
vim.opt.rtp:prepend(vim.fn.stdpath("cache") .. "/thyme/compiled")
vim.loader.enable()

-- thyme reads its options with vim.secure.read, which refuses a file until it is trusted; hjem owns this one
local thyme_config = vim.fs.joinpath(vim.fn.stdpath("config"), ".nvim-thyme.fnl")
if vim.uv.fs_stat(thyme_config) then
  vim.secure.trust({ action = "allow", path = thyme_config })
end

vim.api.nvim_create_autocmd("VimEnter", {
  once = true,
  callback = function()
    vim.schedule(function()
      require("thyme").setup()
    end)
  end,
})

require("moni")
