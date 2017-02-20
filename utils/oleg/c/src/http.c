// vim: noet ts=4 sw=4
#include <arpa/inet.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "http.h"

/* Pulled from here: http://stackoverflow.com/a/25705264 */
static char *strnstr(const char *haystack, const char *needle, size_t len) {
	int i;
	size_t needle_len;

	/* segfault here if needle is not NULL terminated */
	if (0 == (needle_len = strlen(needle)))
		return (char *)haystack;

	/* Limit the search if haystack is shorter than 'len' */
	len = strnlen(haystack, len);

	for (i=0; i<(int)(len-needle_len); i++)
	{
		if ((haystack[0] == needle[0]) && (0 == strncmp(haystack, needle, needle_len)))
			return (char *)haystack;

		haystack++;
	}
	return NULL;
}

int connect_to_host_with_port(const char *host, const char *port) {
	struct addrinfo hints = {
		.ai_flags     = 0,
		.ai_family    = 0,
		.ai_socktype  = 0,
		.ai_protocol  = 0,
		.ai_addrlen   = 0,
		.ai_addr      = 0,
		.ai_canonname = 0,
		.ai_next      = 0
	};
	struct addrinfo *res = NULL;
	int request_fd = 0;

	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;

	int err = 0;
	if ((err = getaddrinfo(host, port, &hints, &res)) != 0) {
		printf("Could not get address information: %s", gai_strerror(err));
		goto error;
	}

	request_fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
	if (request_fd < 0) {
		goto error;
	}

	int opt = 1;
	setsockopt(request_fd, SOL_SOCKET, SO_REUSEADDR, (void*) &opt, sizeof(opt));

	int rc = connect(request_fd, res->ai_addr, res->ai_addrlen);
	if (rc == -1) {
		goto error;
	}

	freeaddrinfo(res);
	return request_fd;

error:
	freeaddrinfo(res);
	close(request_fd);
	return 0;
}

int connect_to_host(const char *host) {
	return connect_to_host_with_port(host, "80");
}

unsigned char *receive_http(const int request_fd, size_t *out) {
	return receive_http_with_timeout(request_fd, SELECT_TIMEOUT, out);
}

char *get_header_value(const char *request, const size_t request_siz, const char header[static 1]) {
	char *data = NULL;
	const char *header_loc = strnstr(request, header, request_siz);
	if (!header_loc)
		return NULL;

	const char *header_value_start = header_loc + strlen(header) + strlen(": ");
	const char *header_value_end = strstr(header_loc, "\r\n");
	if (!header_value_end)
		return NULL;

	const size_t header_value_size = header_value_end - header_value_start + 1;
	data = calloc(header_value_size, sizeof(char));
	strncpy(data, header_value_start, header_value_size);

	return data;
}

char *receieve_only_http_header(const int request_fd, const int timeout, size_t *out) {
	unsigned char *raw_buf = NULL;
	size_t buf_size = 0;
	int times_read = 0;

	fd_set chan_fds;
	FD_ZERO(&chan_fds);
	FD_SET(request_fd, &chan_fds);
	const int maxfd = request_fd;

	struct timeval tv = {
		.tv_sec = timeout,
		.tv_usec = 0
	};
	select(maxfd + 1, &chan_fds, NULL, NULL, &tv);

	unsigned char *header_end = NULL;
	size_t total_received = 0;
	while (1) {
		times_read++;

		int count;
		/* How many bytes should we read: */
		int rc = ioctl(request_fd, FIONREAD, &count);
		if (rc == -1) {
			perror("receieve_only_http_header");
			goto error;
		}

		if (count <= 0)
			break;
		else if (count <= 0) /* Continue waiting. */
			continue;
		int old_offset = buf_size;
		buf_size += count;
		if (raw_buf != NULL) {
			unsigned char *new_buf = realloc(raw_buf, buf_size);
			if (new_buf != NULL) {
				raw_buf = new_buf;
			} else {
				goto error;
			}
		} else {
			raw_buf = calloc(1, buf_size);
		}
		/* printf("IOCTL: %i.\n", count); */

		int received = recv(request_fd, raw_buf + old_offset, count, 0);
		total_received += received;

		/* Attempt to find the header length. */
		if (header_end == NULL) {
			header_end = (unsigned char *)strstr((char *)raw_buf, "\r\n\r\n");
			if (header_end != NULL)
				break;
		}
	}

	if (raw_buf == NULL)
		goto error;

	const size_t header_size = header_end - raw_buf;
	char *to_return = malloc(header_size + 1);
	memcpy(to_return, raw_buf, header_size);
	to_return[header_size] = '\0';
	*out = header_size;
	free(raw_buf);

	return to_return;

error:
	if (raw_buf != NULL)
		free(raw_buf);
	return NULL;
}

unsigned char *receive_http_with_timeout(const int request_fd, const int timeout, size_t *out) {
	unsigned char *raw_buf = NULL;
	size_t buf_size = 0;
	int times_read = 0;

	fd_set chan_fds;
	FD_ZERO(&chan_fds);
	FD_SET(request_fd, &chan_fds);
	const int maxfd = request_fd;


	/* Wait for data to be read. */
	struct timeval tv = {
		.tv_sec = timeout,
		.tv_usec = 0
	};
	select(maxfd + 1, &chan_fds, NULL, NULL, &tv);

	unsigned char *header_end = NULL, *cursor_pos = NULL;
	size_t result_size = 0;
	size_t payload_received = 0;
	size_t total_received = 0;
	while (1) {
		times_read++;

		int count;
		/* How many bytes should we read: */
		int rc = ioctl(request_fd, FIONREAD, &count);
		if (rc == -1) {
			perror("receive_http_with_timeout");
			goto error;
		}

		if (count <= 0 && result_size == payload_received)
			break;
		else if (count <= 0) /* Continue waiting. */
			continue;
		int old_offset = buf_size;
		buf_size += count;
		if (raw_buf != NULL) {
			unsigned char *new_buf = realloc(raw_buf, buf_size);
			if (new_buf != NULL) {
				raw_buf = new_buf;
			} else {
				goto error;
			}
		} else {
			raw_buf = calloc(1, buf_size);
		}
		/* printf("IOCTL: %i.\n", count); */

		int received = recv(request_fd, raw_buf + old_offset, count, 0);
		total_received += received;
		if (header_end)
			payload_received += received;

		/* Attempt to find the header length. */
		if (header_end == NULL) {
			header_end = (unsigned char *)strstr((char *)raw_buf, "\r\n\r\n");
			if (header_end != NULL) {
				cursor_pos = header_end  + (sizeof(char) * 4);
				/* Try to find a 200 or a 201: */

				const char *first_line_end_c = strstr((char *)raw_buf, "\r\n");
				const size_t first_line_end = first_line_end_c - (char *)raw_buf;
				if (strnstr((char *)raw_buf, "200", first_line_end) == NULL && strnstr((char *)raw_buf, "201", first_line_end) == NULL) {
					goto error;
				}

				/* We've received at least part of the payload, add it to the counter. */
				payload_received += total_received - (cursor_pos - raw_buf);

				result_size = 0;
				unsigned char *offset_for_clength = (unsigned char *)strnstr((char *)raw_buf, "Content-Length: ", buf_size);
				if (offset_for_clength == NULL) {
					continue;
				}

				char siz_buf[128] = {0};
				unsigned int i = 0;

				const unsigned char *to_read = offset_for_clength + strlen("Content-Length: ");
				while (to_read[i] != '\r' && to_read[i + 1] != '\n' && i < sizeof(siz_buf)) {
					siz_buf[i] = to_read[i];
					i++;
				}
				result_size = strtol(siz_buf, NULL, 10);
			}
		}
	}

	if (raw_buf == NULL)
		goto error;

	/* Make sure cursor_pos is up to date, no invalidated
	 * pointers or anything. */
	if (header_end != NULL) {
		header_end = (unsigned char *)strstr((char *)raw_buf, "\r\n\r\n");
		cursor_pos = header_end  + (sizeof(char) * 4);
	}

	unsigned char *to_return = malloc(result_size + 1);
	memcpy(to_return, cursor_pos, result_size);
	*out = result_size;
	to_return[result_size] = '\0';
	free(raw_buf);

	return to_return;

error:
	if (raw_buf != NULL)
		free(raw_buf);
	return NULL;
}
