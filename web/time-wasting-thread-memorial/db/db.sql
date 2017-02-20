CREATE TABLE IF NOT EXISTS Posts (
    id     INTEGER PRIMARY KEY
  , oid    TEXT    UNIQUE      NOT NULL
  , body   TEXT                NOT NULL
  , mdown  TEXT                NOT NULL
  , author TEXT                NOT NULL
  , page   INTEGER
);

CREATE TABLE IF NOT EXISTS Users (
    id         INTEGER PRIMARY KEY
  , oid        TEXT    UNIQUE      NOT NULL
  , name       TEXT    UNIQUE      NOT NULL
  , avatar_url TEXT                NOT NULL
);

BEGIN TRANSACTION;

INSERT INTO Users VALUES(NULL, "e4edb497-3f6f-4a7e-8ad4-0272ab8d3a47", "Background Pony", '//derpicdn.net/assets/no_avatar_125x125-2c4e2d8e68cb13a208dae3c6d6877b45c5390dd367920d927c80dde1665bc0ed.png');

COMMIT;
