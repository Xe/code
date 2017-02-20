// vim: noet ts=4 sw=4
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <oleg-http/oleg-http.h>

#define DB_HOST "localhost"
#define DB_PORT "38080"

void usage(const char *name) {
	printf("%s: <db_name> [<key> <key> ...]\nSpecify at least one key.", name);
}

struct db_key_match *construct_keys_from_args(const int argc, char *argv[]) {
	struct db_key_match *head = calloc(1, sizeof(struct db_key_match));
	strncpy((char *)head->key, argv[2], sizeof(head->key));

	struct db_key_match *next = head;

	int i;
	for (i = 3; i < argc; i++) {
		struct db_key_match *new = calloc(1, sizeof(struct db_key_match));
		strncpy((char *)new->key, argv[i], strnlen(argv[i], sizeof(new->key)));
		next->next = new;
		next = new;
	}

	return head;
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

	struct db_key_match *keys = construct_keys_from_args(argc, argv);

	/* Free the keys for us */
	struct db_match *matches = fetch_bulk_from_db(&conn, keys, 1);

	struct db_match *current = matches;
	while (current) {
		struct db_match *next = current->next;

		printf("%s\n", current->data);

		free((char *)current->data);
		free(current);
		current = next;
	}
	return 0;
}
