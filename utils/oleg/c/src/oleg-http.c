// vim: noet ts=4 sw=4
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>

#include "oleg-http.h"
#include "http.h"

#define UINT_LEN(x) floor(log10(x)) + 1

static const char DB_REQUEST[] = "GET /%s/%s HTTP/1.1\r\n"
	"Host: %s:%s\r\n"
	"\r\n";

static const char DB_POST[] = "POST /%s/%s HTTP/1.1\r\n"
	"Host: %s:%s\r\n"
	"Content-Length: %zu\r\n"
	"\r\n"
	"%s";

/* We use 'Accept-Encoding: identity' here so we don't get back chunked
 * transfer shit. I hate parsing that garbage.
 */
static const char DB_MATCH[] = "GET /%s/%s/_match HTTP/1.1\r\n"
	"Host: %s:%s\r\n"
	"Accept-Encoding: identity\r\n"
	"\r\n";

static const char UPTIME_REQUEST[] = "GET /%s/_uptime HTTP/1.1\r\n"
	"Host: %s:%s\r\n"
	"Accept-Encoding: identity\r\n"
	"\r\n";

static const char DB_ALL[] =  "GET /%s/_all HTTP/1.1\r\n"
	"Host: %s:%s\r\n"
	"Accept-Encoding: identity\r\n"
	"\r\n";

static const char DB_BULK_UNJAR[] =  "POST /%s/_bulk_unjar HTTP/1.1\r\n"
	"Host: %s:%s\r\n"
	"Content-Length: %zu\r\n"
	"Accept-Encoding: identity\r\n"
	"\r\n";


static int _fetch_matches_common(const db_conn *conn, const char prefix[static MAX_KEY_SIZE]) {
	const size_t db_match_siz = strlen(conn->db_name) + strlen(conn->host) +
								strlen(conn->port) + strlen(DB_MATCH) +
								strnlen(prefix, MAX_KEY_SIZE);
	char new_db_request[db_match_siz];
	memset(new_db_request, '\0', db_match_siz);

	int sock = 0;
	sock = connect_to_host_with_port(conn->host, conn->port);
	if (sock == 0)
		goto error;

	snprintf(new_db_request, db_match_siz, DB_MATCH, conn->db_name, prefix, conn->host, conn->port);
	unsigned int rc = send(sock, new_db_request, strlen(new_db_request), 0);
	if (strlen(new_db_request) != rc)
		goto error;

	return sock;

error:
	close(sock);
	return 0;
}

static int _send_fetch_all_req(const db_conn *conn) {
	const size_t db_match_siz = strlen(conn->db_name) + strlen(conn->host) +
								strlen(conn->port) + strlen(DB_ALL);
	char new_db_request[db_match_siz];
	memset(new_db_request, '\0', db_match_siz);

	int sock = 0;
	sock = connect_to_host_with_port(conn->host, conn->port);
	if (sock == 0)
		goto error;

	snprintf(new_db_request, db_match_siz, DB_ALL, conn->db_name, conn->host, conn->port);
	unsigned int rc = send(sock, new_db_request, strlen(new_db_request), 0);
	if (strlen(new_db_request) != rc)
		goto error;

	return sock;

error:
	close(sock);
	return 0;
}

unsigned int fetch_num_matches_from_db(const db_conn *conn, const char prefix[static MAX_KEY_SIZE]) {
	size_t outdata = 0;
	char *_data = NULL;
	char *_value = NULL;

	int sock = _fetch_matches_common(conn, prefix);
	if (!sock)
		goto error;

	_data = receieve_only_http_header(sock, SELECT_TIMEOUT, &outdata);
	if (!_data)
		goto error;

	_value = get_header_value(_data, outdata, "X-Olegdb-Num-Matches");
	if (!_value)
		goto error;

	unsigned int to_return = strtol(_value, NULL, 10);

	free(_data);
	free(_value);
	close(sock);
	return to_return;

error:
	free(_value);
	free(_data);
	close(sock);
	return 0;
}

db_key_match *fetch_matches_from_db(const db_conn *conn, const char prefix[static MAX_KEY_SIZE]) {
	size_t dsize = 0;
	unsigned char *_data = NULL;

	int sock = _fetch_matches_common(conn, prefix);
	if (!sock)
		goto error;

	_data = receive_http(sock, &dsize);
	if (!_data)
		goto error;

	db_key_match *eol = NULL;
	db_key_match *cur = eol;
	unsigned int i;
	unsigned char *line_start = _data, *line_end = NULL;
	/* TODO: Use a vector here. */
	for (i = 0; i < dsize; i++) {
		if (_data[i] == '\n' || i == (dsize - 1)) {
			if (i == (dsize - 1))
				line_end = &_data[i + 1];
			else
				line_end = &_data[i];
			const size_t line_size = line_end - line_start;

			db_key_match _stack = {
				.key = {0},
				.next = cur
			};
			memcpy((char *)_stack.key, line_start, line_size);

			db_key_match *new = calloc(1, sizeof(db_key_match));
			memcpy(new, &_stack, sizeof(db_key_match));

			cur = new;
			line_start = &_data[++i];
		}
	}

	free(_data);
	close(sock);
	return cur;

error:
	free(_data);
	close(sock);
	return NULL;
}

unsigned int fetch_num_keyset_from_db(const db_conn *conn) {
	size_t outdata = 0;
	char *_data = NULL;
	char *_value = NULL;

	int sock = _send_fetch_all_req(conn);
	if (!sock)
		goto error;

	_data = receieve_only_http_header(sock, SELECT_TIMEOUT, &outdata);
	if (!_data)
		goto error;

	_value = get_header_value(_data, outdata, "X-Olegdb-Num-Matches");
	if (!_value)
		goto error;

	unsigned int to_return = strtol(_value, NULL, 10);

	free(_data);
	free(_value);
	close(sock);
	return to_return;

error:
	free(_value);
	free(_data);
	close(sock);
	return 0;
}

db_key_match *fetch_keyset_from_db(const db_conn *conn) {
	size_t dsize = 0;
	unsigned char *_data = NULL;

	int sock = _send_fetch_all_req(conn);
	if (!sock)
		goto error;

	_data = receive_http(sock, &dsize);
	if (!_data)
		goto error;

	/* TODO: Refactor this into common function with fetch_matches_from_db */
	db_key_match *eol = NULL;
	db_key_match *cur = eol;
	unsigned int i;
	unsigned char *line_start = _data, *line_end = NULL;
	/* TODO: Use a vector here. */
	for (i = 0; i < dsize; i++) {
		if (_data[i] == '\n' || i == (dsize - 1)) {
			if (i == (dsize - 1))
				line_end = &_data[i + 1];
			else
				line_end = &_data[i];
			const size_t line_size = line_end - line_start;

			db_key_match _stack = {
				.key = {0},
				.next = cur
			};
			memcpy((char *)_stack.key, line_start, line_size);

			db_key_match *new = calloc(1, sizeof(db_key_match));
			memcpy(new, &_stack, sizeof(db_key_match));

			cur = new;
			line_start = &_data[++i];
		}
	}

	free(_data);
	close(sock);
	return cur;

error:
	free(_data);
	close(sock);
	return NULL;
}

unsigned char *fetch_data_from_db(const db_conn *conn, const char key[static MAX_KEY_SIZE], size_t *outdata) {
	unsigned char *_data = NULL;

	const size_t db_request_siz = strlen(conn->db_name) + strnlen(key, MAX_KEY_SIZE) + strlen(conn->host) +
								  strlen(conn->port) + strlen(DB_REQUEST);
	char new_db_request[db_request_siz];
	memset(new_db_request, '\0', db_request_siz);

	int sock = 0;
	sock = connect_to_host_with_port(conn->host, conn->port);
	if (sock == 0)
		goto error;

	snprintf(new_db_request, db_request_siz, DB_REQUEST, conn->db_name, key, conn->host, conn->port);
	unsigned int rc = send(sock, new_db_request, strlen(new_db_request), 0);
	if (strlen(new_db_request) != rc)
		goto error;

	_data = receive_http(sock, outdata);
	if (!_data)
		goto error;

	close(sock);
	return _data;

error:
	free(_data);
	close(sock);
	return NULL;
}

int store_data_in_db(const db_conn *conn, const char key[static MAX_KEY_SIZE], const unsigned char *val, const size_t vlen) {
	unsigned char *_data = NULL;

	const size_t vlen_len = UINT_LEN(vlen);
	/* See DB_POST for why we need all this. */
	const size_t db_post_siz = strlen(conn->db_name) + strlen(key) + strlen(DB_POST) + vlen_len + vlen;
	char new_db_post[db_post_siz + 1];
	memset(new_db_post, '\0', db_post_siz + 1);

	int sock = 0;
	sock = connect_to_host_with_port(conn->host, conn->port);
	if (sock == 0)
		goto error;

	sprintf(new_db_post, DB_POST, conn->db_name, key, conn->host, conn->port, vlen, val);
	unsigned int rc = send(sock, new_db_post, strlen(new_db_post), 0);
	if (strlen(new_db_post) != rc) {
		goto error;
	}

	/* I don't really care about the reply, but I probably should. */
	size_t out;
	_data = receive_http(sock, &out);
	if (!_data) {
		goto error;
	}

	free(_data);
	close(sock);
	return 1;

error:
	free(_data);
	close(sock);
	return 0;
}

db_match *filter(const db_conn *conn, const char prefix[static MAX_KEY_SIZE], const void *extrainput,
		int (*filter)(const unsigned char *data, const size_t dsize, const void *extrainput, void **extradata)) {
	db_match *eol = NULL;
	db_match *cur = eol;

	db_key_match *prefix_matches = fetch_matches_from_db(conn, prefix);
	db_key_match *cur_km = prefix_matches;
	while (cur_km) {
		db_key_match *next = cur_km->next;

		/* 1. Fetch data for this key from DB. */
		size_t dsize = 0;
		unsigned char *_data = fetch_data_from_db(conn, cur_km->key, &dsize);
		/* 2. Apply filter predicate. */
		if (_data) {
			/* 3. If it returns true, add it to the list.*/
			void *extradata = NULL;
			if (filter(_data, dsize, extrainput, &extradata)) {
				db_match _new = {
					.data = _data,
					.dsize = dsize,
					.extradata = extradata,
					.next = cur
				};

				db_match *new = calloc(1, sizeof(db_match));
				memcpy(new, &_new, sizeof(db_match));
				cur = new;
			} else {
				free((unsigned char *)_data);
			}
		}
		/* 4. Continue.*/
		free(cur_km);
		cur_km = next;
	}

	return cur;
}

time_t fetch_uptime_from_db(const db_conn *conn) {
	unsigned char *_data = NULL;
	const size_t uptime_siz = strlen(conn->db_name) + strlen(conn->host) +
								strlen(conn->port) + strlen(UPTIME_REQUEST);
	char new_db_request[uptime_siz];
	memset(new_db_request, '\0', uptime_siz);

	int sock = 0;
	sock = connect_to_host_with_port(conn->host, conn->port);
	if (sock == 0)
		goto error;

	snprintf(new_db_request, uptime_siz, UPTIME_REQUEST,
		conn->db_name, conn->host, conn->port);
	unsigned int rc = send(sock, new_db_request, strlen(new_db_request), 0);
	if (strlen(new_db_request) != rc)
		goto error;

	size_t out;
	_data = receive_http(sock, &out);
	if (!_data) {
		goto error;
	}

	time_t uptime = strtoul((char *)_data, NULL, 10);
	free(_data);
	close(sock);
	return uptime;

error:
	close(sock);
	return 0;
}

static inline size_t _nline_delimited_key_size(const struct db_key_match *keys) {
	size_t siz = 0;

	const db_key_match *current = keys;
	while (current) {
		siz += strlen(current->key);
		current = current->next;
		if (current)
			siz += strlen("\n");
	}

	return siz;
}

static inline db_match *_parse_bulk_response(unsigned char *data, const size_t dsize) {
	db_match *last = NULL;
	db_match *matches = NULL;

	unsigned int i = 0;

	while (i < dsize) {
		char siz_buf[BUNJAR_SIZE_SIZ + 1] = {0};
		unsigned char *data_buf = NULL;

		/* Read in size first */
		unsigned int j = 0;
		for (; j < BUNJAR_SIZE_SIZ; j++)
			siz_buf[j] = data[j + i];

		i += j;

		const long long record_size = strtol(siz_buf, NULL, 10);
		if ((record_size == LONG_MIN || record_size == LONG_MAX) && errno == ERANGE) {
			goto error;
		}

		db_match *actual = NULL;
		if (record_size == 0) {
			actual = calloc(1, sizeof(struct db_match));
			j = 0;
			goto done;
		}

		data_buf = malloc(record_size + 1);
		if (data_buf)
			data_buf[record_size] = '\0';

		for (j = 0; j < record_size; j++)
			data_buf[j] = data[i + j];

		actual = malloc(sizeof(db_match));
		if (!actual) {
			goto error;
		}

		db_match _tmp = {
			.data = data_buf,
			.dsize = record_size,
			.extradata = NULL,
			.next = NULL
		};
		memcpy(actual, &_tmp, sizeof(db_match));

		/* This is dumb but I'm on the train right now and I don't care. */
done:
		if (last)
			last->next = actual;
		if (!matches)
			matches = actual;
		last = actual;
		i += j;
	}

	free(data);
	return matches;

error:
	while (matches) {
		db_match *next = matches->next;
		free((unsigned char *)matches->data);
		free(matches);
		matches = next;
	}
	free(data);

	return NULL;
}

db_match *fetch_bulk_from_db(const db_conn *conn, struct db_key_match *keys, int free_keys) {
	size_t dsize = 0;
	unsigned char *_data = NULL;
	int sock = 0;

	const size_t keys_size = _nline_delimited_key_size(keys);
	const size_t db_bu_siz = strlen(conn->db_name) + strlen(conn->host) +
							 strlen(conn->port) + strlen(DB_BULK_UNJAR);
	const size_t total_size = keys_size + db_bu_siz;
	char *new_db_request = calloc(1, total_size);

	sock = 0;
	sock = connect_to_host_with_port(conn->host, conn->port);
	if (sock == 0) {
		goto error;
	}

	/* TODO: Is it faster here to A) Loop through the keys twice, allocating a single
	 * buffer once the size is known or B) looping through the keys once, allocating a
	 * buffer on the stack and resizing it as we go?
	 */
	snprintf(new_db_request, db_bu_siz, DB_BULK_UNJAR, conn->db_name, conn->host, conn->port, keys_size);
	db_key_match *current = keys;

	while (current) {
		strncat(new_db_request, current->key, total_size);

		struct db_key_match *next = current->next;
		if (next)
			strncat(new_db_request, "\n", total_size);

		if (free_keys)
			free(current);

		current = next;
	}

	unsigned int rc = send(sock, new_db_request, total_size, 0);
	if (total_size != rc) {
		goto error;
	}


	/* Now we get back our weird Oleg-only format of keys. Hopefully
	 * not chunked.
	 */
	_data = receive_http(sock, &dsize);
	if (!_data) {
		goto error;
	}

	free(new_db_request);
	return _parse_bulk_response(_data, dsize);

error:
	if (sock)
		close(sock);
	free(_data);
	free(new_db_request);
	return NULL;
}
