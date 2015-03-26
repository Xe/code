lapis = require "lapis"
csrf = require "lapis.csrf"
encoding = require "lapis.util.encoding"

import capture_errors from require "lapis.application"
import assert_valid from require "lapis.validate"
import respond_to from require "lapis.application"

require "models.user"

class User extends lapis.Application
  [list: "/list"]: =>
    if not @current_user
      @write redirect_to: @url_for "login"

    @list = Users\select!
    render: true

  [register: "/register"]: capture_errors respond_to {
    GET: =>
      @csrf_token = csrf.generate_token @
      render: true

    POST: =>
      csrf.assert_token @
      assert_valid @params, {
        { "email", exists: true, min_length: 3 }
        { "password", exists: true, min_length: 3 }
        { "password_again", equals: @params.password }
        { "name", exists: true, min_length: 3}
      }

      @params.password = encoding.encode_base64 encoding.hmac_sha1(@params.email, @params.password)
      @params.password_again = nil
      @params.csrf_token = nil
      @params.extension = "#{1000}"

      if Users\find email: @params.email
        @title = "Failure"
        return status: 500, "User with that email already exists"

      user = Users\create @params
      user\write_session @

      user.extension = "#{1000 + user.id}"
      user\update "extension"

      token = Tokens\create {
        user_id: user.extension
        token: encoding.encode_base64 encoding.hmac_sha1(@params.email, os.time!)
      }

      @session.token = encoding.encode_base64 token.token

      @session.flash = "You are logged in. Your extension is #{user.extension}."

      redirect_to: @url_for "index"
  }

  [login: "/login"]: capture_errors respond_to {
    GET: =>
      @csrf_token = csrf.generate_token @
      render: true

    POST: =>
      csrf.assert_token @
      assert_valid @params, {
        { "email", exists: true, min_length: 3 }
        { "password", exists: true, min_length: 3 }
      }

      user = Users\find email: @params.email

      cmppass = encoding.encode_base64 encoding.hmac_sha1(@params.email, @params.password)

      if user.password == cmppass
        user\write_session @

        token = Tokens\create {
          user_id: user.extension
          token: encoding.encode_base64 encoding.hmac_sha1(@params.email, os.time!)
        }

        @session.token = encoding.encode_base64 token.token
        @title = "Login successful"

        return "Hi " .. user.name
      else
        @title = "Login failure"
        return status: 500, "bad password"
  }

  [logout: "/logout"]: =>
    @session.user = nil
    @session.sippw = nil

    return redirect_to: "/login"
