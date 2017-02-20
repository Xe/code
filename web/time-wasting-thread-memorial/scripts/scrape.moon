json    = require "dkjson"
sqlite3 = require "lsqlite3"

db = assert sqlite3.open "../db/posts.db"

insert_stmt = assert db\prepare "INSERT INTO Posts VALUES (NULL, ?, ?, '', ?, ?)"

db\exec [[ BEGIN TRANSACTION; ]]

for page=1,8002
  print "Scraping page #{page}..."

  with fin = assert io.open "../raw/pages/#{tostring(page)\sub 1,1}/#{page}.json", "r"
    data = fin\read "*a"
    posts, _, err = json.decode data
    error err unless posts

    for _, post in pairs posts.topics
      continue unless post

      post.author = "e4edb497-3f6f-4a7e-8ad4-0272ab8d3a47" unless post.author

      do
        insert_stmt\bind_values post.id, post.body, post.author, page
        insert_stmt\step!
        insert_stmt\reset!

    fin\close!

  print "done at #{os.date!}"

db\exec [[ COMMIT; ]]
