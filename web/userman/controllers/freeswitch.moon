lapis = require "lapis"

import respond_to from require "lapis.application"

require "models.token"
require "models.user"

class Freeswitch extends lapis.Application
  [freeswitch: "/freeswitch"]: respond_to {
    POST: =>
      uid = @params["user"] or @params["sip_auth_username"]
      @user = Users\find extension: uid

      if not @user
        return status: 404, layout: false, "no such user " .. uid

      @tokens = Tokens\select "where user_id = ?", uid

      if #@tokens == 0
        return status: 404, layout: false, "no auth tokens for " .. uid

      render: true, layout: false
  }
