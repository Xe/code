-- http://lua-users.org/wiki/StringRecipes
function string.starts(String,Start)
  return string.sub(String,1,string.len(Start))==Start
end

function string:split( inSplitPattern, outResults )
  if not outResults then
    outResults = { }
  end

  local theStart = 1
  local theSplitStart, theSplitEnd = string.find( self, inSplitPattern, theStart )

  while theSplitStart do
    table.insert( outResults, string.sub( self, theStart, theSplitStart-1 ) )
    theStart = theSplitEnd + 1
    theSplitStart, theSplitEnd = string.find( self, inSplitPattern, theStart )
  end

  table.insert( outResults, string.sub( self, theStart ) )
  return outResults
end

sqlite3 = require "lsqlite3"

weechat.register("connlog", "Christine Dodrill", "0.1", "MIT", "Logs incoming connections to a sqlite3 database for connlog to parse. Will not work on non-charybdis irc daemons.", "", "")

-- Set up the database handle
db = assert(sqlite3.open(weechat.info_get("weechat_dir", "") .. "/snotelog.db"))

-- Set up table in sqlite
db:exec([[CREATE TABLE IF NOT EXISTS Connections(
  id      INTEGER PRIMARY KEY,
  date    INTEGER,
  exiting INTEGER,
  server  TEXT,
  nick    TEXT,
  ident   TEXT,
  ip      TEXT,
  rdns    TEXT,
  gecos   TEXT
);]])

-- Use a prepared statement to avoid injection risks
insert_stmt = assert(db:prepare("INSERT INTO Connections VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?)"))

-- Hook on printing so we get the server monitor stuff
weechat.hook_print("", "", "", 0, "print_hook", "")

function print_hook(data, buffer, date, tags, displayed, highlight, prefix, message)
  -- Get the local buffer formal name
  local str = weechat.buffer_get_string(buffer, "name")

  -- Server notices use the tags irc_notice and notify_private
  if tags:find("irc_notice,notify_private") then
    -- See if we need to care
    if message:find("Client ") then
      -- *** Notice -- Client connecting: xena_ (xena@10.1.10.11) [10.1.10.11] {users} [Xena]
      local i, j
      local notice
      local exiting = 0

      -- See if the client is connecting or exiting
      if message:find("Client connecting") then
        i, j = message:find("Client connecting: ")
      elseif message:find("Client exiting: ") then
        i, j = message:find("Client exiting: ")
        exiting = 1
      else
        -- otherwise stop caring
        return weechat.WEECHAT_RC_OK
      end

      -- Shuck the string a bit
      notice = message:sub(j):sub(2)

      -- Get the info out of the line
      local splitstuff = notice:split(" ")
      local nick = splitstuff[1]
      local userhost = splitstuff[2]:sub(2,(#splitstuff[2]-1))
      local ip
      local gecos = "<none>"
      local user = userhost:split("@")[1]
      local host = userhost:split("@")[2]

      -- Exit/enter specific logic differences
      if exiting == 0 then
        local x, y
        x, y = notice:find("} %[")
        gecos = notice:sub(y+1)
        gecos = gecos:sub(1,#gecos-1)
        ip = splitstuff[3]:sub(2,(#splitstuff[3]-1))
      else
        local x, y
        x, y = notice:find("%] %[")
        ip = notice:sub(y+1)
        ip = ip:sub(1, #ip-1)
      end

      -- Throw it all into sqlite
      insert_stmt:bind_values(
        os.time(),
        exiting,
        weechat.buffer_get_string(buffer, "short_name"),
        nick,
        user,
        ip,
        host,
        gecos
      )
      insert_stmt:step()
      insert_stmt:reset()
    end
  end

  return weechat.WEECHAT_RC_OK
end
