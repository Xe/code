// vim: noet ts=4 sw=4
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <oleg-http/oleg-http.h>

#define DB_HOST "localhost"
#define DB_PORT "38080"

void usage(const char *name) {
	printf("%s: <db_name> <key>\n", name);
}

int main(int argc, char *argv[]) {
	if (argc < 3) {
		usage(argv[0]);
		return 1;
	}

	struct db_conn conn = {
		.host    = DB_HOST,
		.port    = DB_PORT,
	};
	strncpy(conn.db_name, argv[1], sizeof(conn.db_name));

	size_t dsize = 0;
	unsigned char *data = fetch_data_from_db(&conn, argv[2], &dsize);
	if (data) {
		printf("%s", data);
		free(data);
	} else {
		printf("No such key.");
	}

	return 0;
}
