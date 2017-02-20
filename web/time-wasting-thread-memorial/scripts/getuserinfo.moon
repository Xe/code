https   = require "ssl.https"
json    = require "dkjson"
socket  = require "socket"
sqlite3 = require "lsqlite3"

authors = {
  ["e4edb497-3f6f-4a7e-8ad4-0272ab8d3a47"]: true
}

db = assert sqlite3.open "../db/posts.db"

insert_stmt = assert db\prepare "INSERT INTO Users VALUES (NULL, ?, ?, ?)"

db\trace (ud, sql) ->
  print sql

print [[ BEGIN TRANSACTION; ]]

for row in db\nrows "SELECT * FROM Posts"
  do
    for k,v in pairs row
      print " --  #{k}: #{v}"

  if authors[row.author]
    -- print "Skipping dupe #{row.author}"
    continue

  author = row.author
  authors[author] = true

  print "-- #{os.date!} fetching for #{author}..."

  reply, code, headers = https.request "https://derpibooru.org/profiles/#{author}.json"
  if code ~= 200
    db\exec [[ ROLLBACK; ]]
    error reply

  person, _, err = json.decode reply
  if err
    db\exec [[ ROLLBACK; ]]
    error err

  print "-- #{os.date} found #{person.name}"

  person.avatar = "//derpicdn.net/assets/no_avatar_125x125-2c4e2d8e68cb13a208dae3c6d6877b45c5390dd367920d927c80dde1665bc0ed.png" unless person.avatar

  do
    insert_stmt\bind_values person.id, person.name, person.avatar

    print "-- #{os.date!} adding #{person.id}, #{person.name}"

    err = insert_stmt\step!
    if err ~= sqlite3.OK and err ~= sqlite3.DONE
      print err
      db\exec [[ ROLLBACK; ]]
      error db\errmsg!

    insert_stmt\reset!

  socket.sleep 1

db\exec [[ COMMIT; ]]
