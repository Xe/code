lapis = require "lapis"

require "models.user"

class App extends lapis.Application
  layout: require "layout.bootstrap"

  @include "controllers.users"
  @include "controllers.freeswitch"

  @before_filter =>
    @current_user = Users\read_session @

    if @session.flash
      @flash = @session.flash
      @session.flash = nil

  [index: "/"]: =>
    @name = @session.name
    render: true

  "/debug": =>
    @html ->
      config = require("lapis.config").get!

      p config.port
      p tostring os.getenv "POSTGRESQL_PORT_5432_TCP_PORT"
      p tostring os.getenv "POSTGRESQL_PORT_5432_TCP_ADDR"
      p tostring os.getenv "DB_USER"
      p tostring os.getenv "DB_PASS"
      p tostring os.getenv "DB_NAME"
