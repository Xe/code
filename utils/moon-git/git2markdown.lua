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

function usage()
  print("Usage: " .. arg[0] .. " <base URL> <old tag> <new tag>")
end

function retrieve(baseurl, from, to)
  local gitoutput

  commits = os.capture("git --no-pager log --oneline --no-merges --oneline --format=\"%h %H %s\t\" \""..from.."\"..\""..to.."\"")
  commits = string.split(commits, "\t")

  print("| Message | Link |")
  print("|:--- |:--- |")

  for i, commit in pairs(commits) do
    commit = string.split(commit, " ")

    if commit[1] == "" then
      table.remove(commit, 1)
    end

    local shorthash = table.remove(commit, 1)
    local longhash = table.remove(commit, 1)
    local message = table.concat(commit, " ")

    local link = "[`"..shorthash.."`]("..baseurl.."/commit/"..longhash..")"

    print("| " .. message .. " | " .. link .. " |")
  end
end

if #arg < 3 then
  usage()
  os.exit(1)
end

retrieve(arg[1], arg[2], arg[3])
