import Widget from require "lapis.html"

class List extends Widget
  content: =>
    element "table", ->
      tr ->
        td -> text "id"
        td -> text "name"
        td -> text "email"
        td -> text "admin"
        td -> text "created"
        td -> text "updated"
      for _, user in pairs @list
        tr ->
          td -> text user.id
          td -> text user.name
          td -> text user.email
          td -> text tostring user.admin
          td -> text tostring user.created
          td -> text tostring user.updated
