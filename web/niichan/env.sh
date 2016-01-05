export DATABASE_HOST="$(docker inspect postgres | grep IPA | cut -d'"' -f4)"
export DATABASE_PORT='5432'
export DATABASE_USER='niichan'
export DATABASE_PASSWORD='foobang'
export DATABASE_NAME='niichandata'
