function Initialize(Plugin)
  Plugin:SetName "XeCommands"
  Plugin:SetVersion(0)

  PLUGIN = Plugin

  cPluginManager.BindCommand("/join", "*", JoinCommand, " ~ joins you to a specific world by name")

  LOG("Initialised " .. Plugin:GetName() .. " v." .. Plugin:GetVersion())

  return true
end

function OnDisable()
  LOG(PLUGIN:GetName() .. " is shutting down...")
end

function JoinCommand(split, player)
  local root = cRoot:Get()
  local world = root:GetWorld(split[2])

  if world then
    player:MoveToWorld(world, false)
    player:SendMessageInfo("Teleported you to world " .. world:GetName())

    player:SetPosition(world:GetSpawnX(), world:GetSpawnY(), world:GetSpawnZ())
  else
    player:SendMessageFailure("Could not find world " .. split[2])
  end

  return true
end
