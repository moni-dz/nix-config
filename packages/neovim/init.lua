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
