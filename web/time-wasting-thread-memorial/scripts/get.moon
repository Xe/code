require "socket"

for i=1,8002
  os.execute "curl https://derpibooru.org/dis/the-time-wasting-thread/#{i}.json?key=#{os.getenv "DB_KEY"} > #{i}.json"
  socket.sleep 1
