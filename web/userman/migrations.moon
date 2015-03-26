import add_column, create_table, drop_column, types from require "lapis.db.schema"

{
  [1]: =>
    create_table "users", {
      { "id", types.serial }
      { "email", types.text }
      { "name", types.text }
      { "password", types.text }

      "PRIMARY KEY (id)"
    }

  [2]: =>
    add_column "users", "admin", types.boolean

  [3]: =>
    add_column "users", "extension", types.text

  [4]: =>
    add_column "users", "created_at", types.time
    add_column "users", "updated_at", types.time

  [5]: =>
    add_column "users", "is_agent", types.boolean

  [6]: =>
    add_column "users", "registrar_password", types.text

  [7]: =>
    create_table "tokens", {
      { "id", types.serial }
      { "user_id", types.integer }
      { "token", types.text }
    }

  [8]: =>
    drop_column "users", "registrar_password"
}
