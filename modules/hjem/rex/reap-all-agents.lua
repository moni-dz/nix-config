local reaper_script = (os.getenv("HOME") or "") .. "/.claude/bin/agent-reaper.lua"

local function server_rex()
  local r, err = rex.exec({ "sh", "-c", 'ps -o comm= -p "$PPID"' })
  if r == nil then return nil, err end
  local path = r.stdout:gsub("%s+$", "")
  if not r.ok or path == "" then return nil, "could not find the server executable: " .. r.stderr end
  return path
end

local function start_reaper(live)
  return function(ctx, args)
    local bin, err = server_rex()
    if bin == nil then error(err, 0) end
    local cmd = { bin, "do", reaper_script, "--all" }
    if live then cmd[#cmd + 1] = "dry=false" end
    local dir = bin:match("^(.*)/[^/]*$")
    local h, serr = rex.exec.start(cmd, {
      detach = true,
      env = { PATH = dir .. ":" .. (os.getenv("PATH") or "/usr/bin:/bin") },
    })
    if h == nil then error(serr, 0) end
  end
end

rex.action{
  name     = "agent_reaper_live",
  title    = "Reap All Agents",
  category = "Agents",
  run      = start_reaper(true),
}
