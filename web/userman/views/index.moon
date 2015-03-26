import Widget from require "lapis.html"

class Index extends Widget
  content: =>
    h1 class: "header", "Hello"
    div class: "body", ->
      text "Welcome to my site!"

    if @name
      h2 "I think you are #{@name}"
