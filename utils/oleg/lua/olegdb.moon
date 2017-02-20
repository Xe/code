ltn12 = require "ltn12"
http  = require "socket.http"

-- Package olegdb implements a client for OlegDB.
module "olegdb", package.seeall
export ^

class Database
  new: (host, port) =>
    @host = host
    @port = port

  Open: (name) =>
    return Table @, name

class Table
  new: (parent, name) =>
    @parent = parent
    @name = name

  Get: (name) =>
    data, code = http.request {
      url:    "http://#{@parent.host}:#{@parent.port}/#{@name}/#{name}"
      method: "GET"
    }

    if code ~= 200
      return nil, data

    data, nil

  Set: (name, value) =>
    data, code = http.request {
      url:    "http://#{@parent.host}:#{@parent.port}/#{@name}/#{name}"
      method: "POST"
      source: ltn12.source.string tostring value
      headers: {
        ["content-length"]: #tostring value
      }
    }

    if code ~= 200
      return nil, data

    data, nil

  Delete: (name) =>
    data, code = http.request {
      url:    "http://#{@parent.host}:#{@parent.port}/#{@name}/#{name}"
      method: "DELETE"
    }

    if code ~= 200
      return data

    nil
