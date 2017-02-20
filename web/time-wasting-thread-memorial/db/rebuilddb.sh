#!/bin/bash

set -e
set -x

rm ./posts.db
cat ./db.sql | sqlite3 ./posts.db
