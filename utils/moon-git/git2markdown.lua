-- http://stackoverflow.com/a/326715
function os.capture(cmd, raw)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  f:close()

  if raw then
    return s
  end

  s = string.gsub(s, '^%s+', '')
  s = string.gsub(s, '%s+$', '')
  s = string.gsub(s, '[\n\r]+', ' ')
  return s
end

-- http://lua-users.org/wiki/SplitJoin
function string:split(sep)
  local sep, fields = sep or ":", {}
  local pattern = string.format("([^%s]+)", sep)
  self:gsub(pattern, function(c) fields[#fields+1] = c end)
  return fields
end

types = "feat(;Features:fix(;Fixes:docs(;Documentation:chore(;Maintenance"
baseurl = "http://github.com/deis/deis"

function usage()
  print("Usage: " .. arg[0] .. " <from> [to]")
  print("Current types and baseurl settings:\n")
  print("    types: " .. types)
  print("    baseurl: " .. baseurl)
  print("override these with --baseurl= and --types=")
end

function retrieve(kind, from, to)
  local gitoutput

  commits = os.capture("git --no-pager log --oneline --no-merges --oneline --format=\"%h %H %s\t\" --grep=\""..kind.."\" \""..from.."\"..\""..to.."\"")
  commits = string.split(commits, "\t")

  for i, commit in pairs(commits) do
    print(commit)
    commit = string.split(commit, " ")
    local subsystem = string.split(commit[3], "%(")[2]
    subsystem = string.split(subsystem, ")")[1]

    local message = string.gsub(commit[3], "%b()+", "")
    print(message)
    message = string.sub(message, 2)
    print(message)

    local link = baseurl.."/commit/"..commit[2]
    print(link)
  end
end

if #arg < 1 then
  print("Need a tag to compare against")
  usage()
  os.exit(1)
end

retrieve("feat(", "v0.9.0", "HEAD")
