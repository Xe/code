import Widget from require "lapis.html"

class LoginForm extends Widget
  content: =>
    form method: "POST", action: "/login", ->
      p ->
        text "email address"
      input type: "hidden", name: "csrf_token", value: @csrf_token
      input type: "text", name: "email"
      p ->
        text "password"
      input type: "password", name: "password"
      br!
      input type: "submit"
