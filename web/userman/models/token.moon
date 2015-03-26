db = require "lapis.db"
import Model from require "lapis.db.model"

require "models.user"

export class Tokens extends Model
  get_user: =>
    return Users\find extension @user_id
