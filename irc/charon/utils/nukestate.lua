local n = 0

for _, k in ipairs(redis.call("KEYS", "charon:user:*")) do
  redis.call("DEL", k)

  n = n + 1
end

return n
