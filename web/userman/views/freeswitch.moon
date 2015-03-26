import Widget from require "lapis.html"

class Freeswitch extends Widget
  content: =>
    document type: "freeswitch/xml", ->
      section name: "directory", ->
        domain name: "$${domain}", ->
          params ->
            param name: "dial-string", value: "{presence_id=${dialed_user}@${dialed_domain}}${sofia_contact(${dialed_user}@${dialed_domain})}"
          groups ->
            group name: "default", ->
              users ->
                for _, token in pairs @tokens
                  user id: "#{@user.extension}", ->
                    params ->
                      param name: "password", value: "#{token.token}"
                    variables ->
                      variable name: "accountcode", value: "#{@user.extension}"
                      variable name: "user_context", value: "default"

                  token\delete!
