import Widget from require "lapis.html"

class LoginForm extends Widget
  content: =>
    form method: "POST", action: "/register", ->
      input type: "hidden", name: "csrf_token", value: @csrf_token

      p "email address"
      input type: "text", name: "email"

      p "password"
      input type: "password", name: "password"

      p "password"
      input type: "password", name: "password_again"

      p "name"
      input type: "text", name: "name"

      br!

      input type: "submit"
