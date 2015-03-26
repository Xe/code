import Model from require "lapis.db.model"

export class Users extends Model
  @timestamp: true

  write_session: (r) =>
    r.session.user = @id
    r.session.name = @name

  read_session: (r) =>
    if r.session.user
      return @find id: r.session.user
    nil
