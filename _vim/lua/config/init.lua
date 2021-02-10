local core = require("aniseed.core")
local nvim = require("aniseed.nvim")
local util = require("config.util")
local function _1_(path)
  return require(string.gsub(path, ".*/(.-)/(.-)/(.-)%.lua", "%1.%2.%3"))
end
core["run!"](_1_, util.glob((util["config-path"] .. "/lua/config/module/*.lua")))
return {["aniseed/module"] = "config.init"}