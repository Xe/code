import httpclient, osproc

when isMainModule:
  try:
    discard getContent "https://twtxtlist.cf"
    echo "Caddy is okay"

  except:
    let p = startProcess("/bin/systemctl", args=["restart", "caddy"])
    discard p.waitForExit()
    echo "Caddy restarted"
