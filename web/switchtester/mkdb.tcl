#!/bin/env wapptclsh

sqlite3 db $::env(DATABASE_PATH)

db eval {
    CREATE TABLE IF NOT EXISTS systemmates
      ( name TEXT UNIQUE
      );
}

db eval {
    CREATE TABLE IF NOT EXISTS observers
      ( name TEXT UNIQUE
      , discord_tag TEXT UNIQUE
      );
}

db eval {
    CREATE TABLE IF NOT EXISTS measurements
      ( id INTEGER PRIMARY KEY AUTOINCREMENT
      , ts INTEGER
      , observer_id INTEGER NOT NULL
      , systemmate_id INTEGER NOT NULL
      , actual_systemmate_id INTEGER NOT NULL
      , suspect_at INTEGER
      , FOREIGN KEY(observer_id) REFERENCES observers(rowid)
      , FOREIGN KEY(systemmate_id) REFERENCES systemmates(rowid)
      , FOREIGN KEY(actual_systemmate_id) REFERENCES systemmates(rowid)
      );
}

# Seed the database
db eval {
    INSERT INTO
      systemmates(name)
    VALUES
      ('Cadey')
    , ('Nicole')
    , ('Jessie')
    , ('Ashe')
    , ('Sephie')
    ;
}
