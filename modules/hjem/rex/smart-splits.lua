-- true when the pane's foreground process is Neovim, so its keys must fall through to it
local function is_vim(ctx)
  local proc = rex.block.call('com.superlogical.terminal', 'process', { block_id = ctx.block_id })
  local fg = proc and proc.foreground
  if not fg then
    return false
  end
  -- `name` is not always the executable's name, so check `argv0` too
  for _, s in ipairs({ fg.name or '', (fg.argv0 or ''):match('[^/]*$') }) do
    if s:match('^n?vim') then
      return true
    end
  end
  return false
end

for key, direction in pairs({ h = 'left', j = 'down', k = 'up', l = 'right' }) do
  rex.bind('ctrl+' .. key, function(ctx)
    if is_vim(ctx) then
      return false
    end
    rex.session.focus_direction({ direction = direction })
    return true
  end)
end

-- rex.client.queue only works inside actions, so this forwards the key to Neovim itself
rex.action({
  name = 'smart_splits_resize',
  title = 'Smart Splits: Resize Pane',
  run = function(ctx, args)
    if is_vim(ctx) then
      rex.client.queue('pane.send_key', { key = args.key })
    else
      rex.client.queue('pane.resize', { direction = args.direction, amount = 3 })
    end
  end,
})

for key, direction in pairs({ h = 'left', j = 'down', k = 'up', l = 'right' }) do
  rex.bind('alt+' .. key, 'smart_splits_resize', { key = 'alt+' .. key, direction = direction })
end
