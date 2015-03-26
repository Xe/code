import Widget from require "lapis.html"

class Layout extends Widget
  head: =>
    meta charset: "UTF-8"

    title ->
      if @title
        text "#{@title} - Userman"
      else
        text "Userman"

    if @meta_description
      meta property: "og:description", content: @meta_description
      meta name: "description", content: @meta_description

    link rel: "stylesheet", href: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"
    link rel: "stylesheet", href: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap-theme.min.css"
    link rel: "stylesheet", href: "/static/css/main.css"

    if @needs and @needs.css
      for _, sheet in pairs @needs.css
        link rel: "stylesheet", href: sheet

    script src: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/js/bootstrap.min.js"

  header: =>
    nav class: "navbar navbar-inverse navbar-fixed-top", ->
      div class: "container", ->
        div class: "navbar-header", ->
          button type: "button", class: "navbar-toggle collapsed", ["data-toggle"]: "collapse", ["data-target"]: "#navbar", ["aria-expanded"]: "false", ["aria-controls"]: "navbar", ->

            span class: "sr-only", ->
              text "Toggle navigation"

            for i=1,3
              span class: "icon-bar"

          a class: "navbar-brand", href: "/", ->
            text "Userman"

        div id: "navbar", class: "collapse navbar-collapse", ->
          ul class: "nav navbar-nav", ->
            li ->
              a href: "/login", "Login"
            li ->
              a href: "/list", "List"
            li ->
              a href: "/register", "Register"

  footer: =>
    footer class: "footer", ->
      div class: "container", ->
        if @copyright
          p class: "text-muted", ->
            text @copytight
        else
          p class: "text-muted", ->
            text "Copyright #{os.date "%Y"}"

  body: =>
    div class: "container", ->
      @content_for "inner"

  content: =>
    html_5 ->
      head ->
        @head!

      body ->
        @header!

        if @flash
          div class: "flash", ->
            text @flash

        @body!
        @footer!
