require "socket"
https = require "ssl.https"
json = require "dkjson"

username = (os.getenv "GITHUB_USERNAME") or "Xe"

repodata, code = https.request "https://api.github.com/users/#{username}/repos"
if code ~= 200
  print code
  error repodata

repos = json.decode repodata

for id, repo in pairs repos
  continue if repo.fork
  print "## #{repo.full_name}\n"
  print "Language: #{repo.language}\nCreated At: #{repo.created_at}"

  do
    rd, code = https.request repo.url
    if code ~= 200
      error rd

    erepo = json.decode rd

    print "\n#{erepo.description}\n\nFind out more at #{erepo.html_url}\n"

  print "---\n"

