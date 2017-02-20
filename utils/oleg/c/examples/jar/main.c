// vim: noet ts=4 sw=4
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <oleg-http/oleg-http.h>

#define DB_HOST "localhost"
#define DB_PORT "38080"

void usage(const char *name) {
	printf("%s: <db_name> <key> <value>\n", name);
}

int main(int argc, char *argv[]) {
	if (argc < 4) {
		usage(argv[0]);
		return 1;
	}

	struct db_conn conn = {
		.host    = DB_HOST,
		.port    = DB_PORT,
	};
	strncpy(conn.db_name, argv[1], sizeof(conn.db_name));

	const int rc = store_data_in_db(&conn, argv[2], (unsigned char *)argv[3], strlen(argv[3]));
	if (!rc) {
		printf("Something went wrong.\n");
		return 1;
	}

	return 0;
}
