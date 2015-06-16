#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curses.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#define BUFSIZE  2048
#define LINESIZE 2048
#define MAXLINES 300

#define LINE_DEBUG       0
#define LINE_INFO        1
#define LINE_ERROR       2
#define LINE_IN          3
#define LINE_OUT         4
#define LINE_TYPES       5

struct line {
	char str[LINESIZE];
	size_t len;
	struct line *next;
	unsigned long type;
};

struct input {
	const char *prompt;
	size_t promptlen;
	char buf[LINESIZE];
	size_t len, size, cur;
};

struct buf {
	char buf[BUFSIZE];
	size_t len, size;
};

struct line *line_head=NULL, *line_tail=NULL;
size_t line_count=0;

WINDOW *win;
int attr_bar;

struct input in_primary, in_command;
struct input *input;

char sock_params[BUFSIZE] = "";
char sock_info[LINESIZE] = "(not connected)";
int sock = -1;
const char *delim = "\r\n";
struct buf bufin;
struct buf bufout;

int running;

struct type_info {
	const char *prefix;
	short f, b;
	int extra;
	int attr; /* calculated in init_curses */
} type_tbl[] = {
	[LINE_DEBUG] = { " - ", COLOR_BLACK,  COLOR_BLACK, A_BOLD },
	[LINE_INFO]  = { " - ", COLOR_WHITE,  COLOR_BLACK, 0 },
	[LINE_ERROR] = { "=!=", COLOR_WHITE,  COLOR_BLACK, 0 },
	[LINE_IN]    = { "-->", COLOR_YELLOW, COLOR_BLACK, 0 },
	[LINE_OUT]   = { "<--", COLOR_CYAN,   COLOR_BLACK, 0 },
};

int init_curses(void)
{
	int i;
	short pair;

	win = initscr();
	if (win == NULL || start_color() != OK)
		return -1;

	pair = 0;
	for (i=0; i<LINE_TYPES; i++) {
		type_tbl[i].attr = type_tbl[i].extra;
		if (has_colors()) {
			init_pair(++pair, type_tbl[i].f, type_tbl[i].b);
			type_tbl[i].attr |= COLOR_PAIR(pair);
		}
	}

	if (has_colors()) {
		init_pair(++pair, COLOR_WHITE, COLOR_BLUE);
		attr_bar = COLOR_PAIR(pair);
	} else {
		attr_bar = A_REVERSE;
	}

	keypad(win, true);
	nodelay(win, true);
	raw();
	noecho();

	return 0;
}

void deinit_curses(void)
{
	noraw();
	echo();
	endwin();
}

/* use line_alloc please, thx */
struct line *line_new(void)
{
	return (struct line*)malloc(sizeof(struct line));
}

struct line *line_alloc(void)
{
	struct line *ln;

	if (line_count == 0) {
		line_head = line_new();
		line_head->next = line_head;
		line_count++;
		return line_head;
	}

	if (line_count == 1) {
		line_tail = line_new();
		line_head->next = line_tail;
		line_tail->next = line_head;
		line_count++;
		return line_tail;
	}

	if (line_count >= MAXLINES) {
		line_head = line_head->next;
		line_tail = line_tail->next;
		return line_tail;
	}

	ln = line_new();
	ln->next = line_head;
	line_tail->next = ln;
	line_tail = ln;
	line_count++;

	return ln;
}

void line_put(unsigned long type, const char *fmt, ...)
{
	struct line *ln = line_alloc();
	va_list va;

	va_start(va, fmt);
	ln->len = vsnprintf(ln->str, LINESIZE, fmt, va);
	va_end(va);

	ln->type = type;
}

int connect_to(const char *host, const char *port)
{
	char buf[BUFSIZE];
	struct addrinfo hints;
	struct addrinfo *addr, *cur;
	void *in;
	int err;

	if (sock != -1) {
		line_put(LINE_ERROR, "Already connected!");
		return -1;
	}

	line_put(LINE_INFO, "Connecting to %s:%s...", host, port);
	snprintf(sock_params, BUFSIZE, "%s %s", host, port);

	snprintf(sock_info, LINESIZE, "(not connected)");
	sock = -1;

	hints.ai_flags = 0;
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = 0;
	if ((err = getaddrinfo(host, port, &hints, &addr)) != 0) {
		line_put(LINE_ERROR, "Could not resolve %s:%s! %s", host, port,
		         gai_strerror(err));
		return -1;
	}

	for (cur=addr; cur; cur=cur->ai_next) {
		switch (cur->ai_family) {
		case AF_INET:
			in = &((struct sockaddr_in*)cur->ai_addr)->sin_addr;
			break;
		case AF_INET6:
			in = &((struct sockaddr_in6*)cur->ai_addr)->sin6_addr;
			break;
		default:
			in = NULL;
			strcpy(buf, "?");
		}
		if (in != NULL)
			inet_ntop(cur->ai_family, in, buf, BUFSIZE);
		snprintf(sock_info, LINESIZE, "%s:%s [%s]", host, port, buf);
		line_put(LINE_DEBUG, "Attempting %s", sock_info);

		if ((sock = socket(cur->ai_family, SOCK_STREAM, 0)) < 0) {
			err = errno;
			line_put(LINE_ERROR, "Failed to create socket! %s",
			         strerror(err));
			freeaddrinfo(addr);
			sock = -1;
			return -1;
		}

		if (connect(sock, cur->ai_addr, cur->ai_addrlen) < 0) {
			err = errno;
			line_put(LINE_DEBUG, "connect() failed! %s", strerror(err));
			close(sock);
			sock = -1;
		} else {
			break;
		}
	}

	if (cur == NULL) {
		line_put(LINE_ERROR, "Could not connect!");
		freeaddrinfo(addr);
		return -1;
	}

	line_put(LINE_INFO, "Connected!");
	freeaddrinfo(addr);
	return 0;
}

void disconnect(void)
{
	if (sock == -1) {
		line_put(LINE_ERROR, "Not connected");
		return;
	}

	line_put(LINE_INFO, "Disconnecting");
	snprintf(sock_info, LINESIZE, "(not connected)");
	close(sock);
	sock = -1;
}

void do_command(char *buf)
{
	char *s, *p;
	char tok[BUFSIZE];

	snprintf(tok, BUFSIZE, "%s", buf);
	s = strtok(buf, " ");
	if (!s)
		return;

	for (p=s; *p; p++)
		*p = toupper(*p);

	if (!strcmp(s, "CONNECT")) {
		s = strtok(NULL, " ");
		if (s != NULL)
			p = strtok(NULL, " ");
		if (!s || !p) {
			line_put(LINE_ERROR, "Not enough arguments to CONNECT");
		} else {
			connect_to(s, p);
		}

	} else if (!strcmp(s, "DISCONNECT")) {
		disconnect();

	} else if (!strcmp(s, "RECONNECT")) {
		if (sock_params[0] == '\0') {
			line_put(LINE_ERROR, "No previous connection");
			return;
		}

		snprintf(tok, BUFSIZE, "%s", sock_params);
		s = strtok(tok, " ");
		if (s != NULL)
			p = strtok(NULL, " ");
		if (!s || !p) {
			line_put(LINE_ERROR, "sock_params had bad data! "
			         "This is a bad thing!");
		} else {
			connect_to(s, p);
		}

	} else if (!strcmp(s, "QUIT") || !strcmp(s, "EXIT")) {
		running = 0;

	} else if (!strcmp(s, "CRLF")) {
		s = strtok(NULL, " ");

		if (s == NULL) {
			if (!strcmp(delim, "\r\n"))
				line_put(LINE_INFO, "Current delimiter is \\r\\n");
			else
				line_put(LINE_INFO, "Current delimiter is \\n");
		} else if (!strcasecmp(s, "ON")) {
			delim = "\r\n";
			line_put(LINE_INFO, "Delimiter changed to \\r\\n");
		} else if (!strcasecmp(s, "OFF")) {
			delim = "\n";
			line_put(LINE_INFO, "Delimiter changed to \\n");
		} else {
			line_put(LINE_ERROR, "Unknown delimiter subcommand (use ON or OFF)");
		}

	} else if (!strcmp(s, "HELP")) {
		line_put(LINE_INFO, "Netline commands:");
		line_put(LINE_INFO, "  CONNECT <HOST> <PORT>   Connect to the specified location");
		line_put(LINE_INFO, "  DISCONNECT              Close socket connection");
		line_put(LINE_INFO, "  RECONNECT               Reconnect using last parameters to CONNECT");
		line_put(LINE_INFO, "  QUIT, EXIT              Leave netline (we'll miss you!)");
		line_put(LINE_INFO, "  HELP                    Show this help");
		line_put(LINE_INFO, "  CRLF [ON|OFF]           Toggle line ending (between \\r\\n and \\n)");
		line_put(LINE_INFO, "When connected, simply type lines to send them");
		line_put(LINE_INFO, "^C may be used at any time to terminate netline");

	} else {
		line_put(LINE_ERROR, "Unknown command %s", s);
	}
}

void input_reset(struct input *in)
{
	in->len = in->cur = 0;
	in->buf[0] = '\0';
}

void input_init(struct input *in, const char *prompt)
{
	in->prompt = prompt;
	in->promptlen = strlen(in->prompt);
	in->size = LINESIZE - 1;
	input_reset(in);
}

int input_process(struct input *in, int ch)
{
	static char kbuf[LINESIZE];
	static size_t klen = 0;
	int i;
	char *s;

	switch (ch) {
	case KEY_HOME:
	case 1: /* ^A */
		in->cur = 0;
		break;
	case KEY_END:
	case 5: /* ^E */
		in->cur = in->len;
		break;

	case KEY_LEFT:
	case 2: /* ^B */
		if (in->cur-- == 0)
			in->cur = 0;
		break;
	case KEY_RIGHT:
	case 6: /* ^F */
		if (in->cur++ >= in->len)
			in->cur = in->len;
		break;

	case KEY_BACKSPACE:
	case 127: /* DEL */
	case 8: /* ^H */
		if (in->cur-- == 0) {
			in->cur = 0;
			break;
		}
		/* fall through */
	case KEY_DC:
	case 4: /* ^D */
		if (in->len == 0)
			break;

		in->len--;
		for (i=in->cur; i<in->len; i++)
			in->buf[i] = in->buf[i+1];
		break;

	case KEY_EOL:
	case 11: /* ^K */
		klen = in->len - in->cur;
		memcpy(kbuf, in->buf + in->cur, klen);
		in->len -= klen;
		break;

	case 21: /* ^U */
		klen = in->cur;
		memcpy(kbuf, in->buf, klen);
		for (i=0; i<klen; i++)
			in->buf[i] = in->buf[i+klen];
		in->len -= klen;
		in->cur = 0;
		break;

	case 23: /* ^W */
		s = in->buf + in->cur;
		while (s > in->buf && isspace(s[-1]))
			s--;
		while (s > in->buf && !isspace(s[-1]))
			s--;
		klen = (in->buf + in->cur) - s;
		memcpy(kbuf, s, klen);
		for (i=0; i<klen; i++)
			s[i] = s[i+klen];
		in->len -= klen;
		in->cur -= klen;
		break;

	case 25: /* ^Y */
		if (in->len + klen > in->size) {
			line_put(LINE_ERROR, "Kill buffer too big!");
			return -1;
		}

		for (i=in->len; i>in->cur; i--)
			in->buf[i+klen-1] = in->buf[i-1];
		for (i=0; i<klen; i++)
			in->buf[in->cur+i] = kbuf[i];
		in->len += klen;
		in->cur += klen;
		break;

	case 0: /* ^@ */
	case 3: /* ^C */
	case 7: /* ^G */
	case 9: /* ^I */
	case 10: /* ^J */
	case 12: /* ^L */
	case 13: /* ^M */
	case 14: /* ^N */
	case 15: /* ^O */
	case 16: /* ^P */
	case 17: /* ^Q */
	case 18: /* ^R */
	case 19: /* ^S */
	case 20: /* ^T */
	case 22: /* ^V */
	case 24: /* ^X */
	case 26: /* ^Z */
	case 27: /* ^3 */
	case 28: /* ^4 */
	case 29: /* ^5 */
	case 30: /* ^6 */
	case 31: /* ^7 */
		return ch;

	default:
		if (ch < 32 || ch >= 127) /* control char */
			return ch;
		if (in->len >= in->size)
			break;

		for (i=in->len++; i>in->cur; i--)
			in->buf[i] = in->buf[i-1];
		in->buf[in->cur++] = ch;
	}

	if (in->cur > in->len)
		in->cur = in->len;
	in->buf[in->len] = '\0';

	return -1;
}

void println(int row, int w, int attr, const char *fmt, ...)
{
	char buf[BUFSIZE];
	int bufsize;
	va_list va;

	bufsize = BUFSIZE - 1;
	if (bufsize > w)
		bufsize = w;

	va_start(va, fmt);
	vsnprintf(buf, bufsize+1, fmt, va);
	va_end(va);

	move(row, 0);
	attron(attr);
	printw("%-*s", w, buf);
	attroff(attr);
}

void display(void)
{
	int i, n, w, h;
	struct type_info *type;
	struct line *ln;

	getmaxyx(win, h, w);

	/* row 0 is title */
	println(0, w, attr_bar, "Type ^X to get the netline prompt, and ^C to quit at any time");

	/* row 1 to h-3 is lines */
	ln = line_head;
	n = ((int)line_count) - (h-3);
	for (i=0; ln && i<n; i++)
		ln = ln->next;
	for (i=1; i<h-2; i++) {
		if (ln != NULL) {
			type = &type_tbl[ln->type];
			println(i, w, type->attr, "%s %s", type->prefix, ln->str);
			ln = ln->next;
			if (ln == line_head)
				ln = NULL;
		} else {
			println(i, w, type_tbl[LINE_INFO].attr, "");
		}
	}

	/* row h-2 is status */
	println(h-2, w, attr_bar, "%s", sock_info);

	/* row h-1 is input */
	println(h-1, w, 0, "%s%s", input->prompt, input->buf);
	move(h-1, input->promptlen + input->cur);

	refresh();
}

void send_line(const char *line)
{
	int sz;

	if (sock == -1) {
		line_put(LINE_ERROR, "Not connected!");
		return;
	}

	line_put(LINE_OUT, "%s", line);
	sz = snprintf(bufout.buf+bufout.len, bufout.size-bufout.len,
	              "%s%s", line, delim);
	bufout.len += sz;
}

void process_input(int ch)
{
	if (ch == 3) {
		running = 0;
		return;
	}

	switch (ch) {
	case KEY_RESIZE:
	case KEY_MOUSE:
		return;
	}

	ch = input_process(input, ch);

	if (ch == -1)
		return;

	switch (ch) {
	case 10:
	case 13:
		if (input == &in_primary) {
			send_line(input->buf);
			input_reset(input);
		} else {
			do_command(input->buf);
			input_reset(input);
			input = &in_primary;
		}
		break;

	case 12:
		redrawwin(win);
		break;

	case 24:
		input = (input == &in_primary) ? &in_command : &in_primary;
		if (input == &in_command)
			input_reset(input);
		break;

	default:
		line_put(LINE_DEBUG, "Unhandled char %d", ch);
	}
}

void buf_reset(struct buf *buf)
{
	buf->len = 0;
	buf->size = BUFSIZE - 1;
	buf->buf[0] = '\0';
}

void select_fd(int fd, fd_set *fds, int *maxfd)
{
	if (fd + 1 > *maxfd)
		*maxfd = fd + 1;
	FD_SET(fd, fds);
}

void lines(unsigned long type, struct buf *buf)
{
	char *s, *end;

	while (buf->len > 0) {
		s = memchr(buf->buf, '\r', buf->len);
		if (s == NULL)
			s = memchr(buf->buf, '\n', buf->len);
		if (s == NULL)
			break;

		end = buf->buf + buf->len;
		while (s < end && (*s == '\n' || *s == '\r'))
			*s++ = '\0';

		line_put(type, "%s", buf->buf);
		if (end != s)
			memmove(buf->buf, s, end - s);
		buf->len -= s - buf->buf;
	}
}

void evloop()
{
	fd_set r, w, x;
	struct timeval tv;
	int ch, maxfd, err;
	ssize_t sz;

	FD_ZERO(&x);

	for (running=1; running;) {
		lines(LINE_IN, &bufin);
		display();

		FD_ZERO(&r);
		FD_ZERO(&w);

		maxfd = 0;

		select_fd(0, &r, &maxfd);

		if (sock != -1) {
			select_fd(sock, &r, &maxfd);
			if (bufout.len > 0)
				select_fd(sock, &w, &maxfd);
		}

		tv.tv_sec = 0;
		tv.tv_usec = 100000;

		if (select(maxfd, &r, &w, &x, &tv) < 0)
			continue;

		if (FD_ISSET(0, &r)) {
			while ((ch = getch()) != ERR)
				process_input(ch);
		}

		if (sock == -1)
			continue;

		if (FD_ISSET(sock, &w)) {
			sz = write(sock, bufout.buf, bufout.len);
			if (sz < 0) {
				line_put(LINE_ERROR, "write() error");
				disconnect();
			} else {
				memmove(bufout.buf, bufout.buf + sz, bufout.len - sz);
				bufout.len -= sz;
			}
		}
		if (FD_ISSET(sock, &r)) {
			sz = read(sock, bufin.buf+bufin.len, bufin.size-bufin.len);
			if (sz <= 0) {
				if (sz < 0)
					line_put(LINE_ERROR, "read() error");
				else
					line_put(LINE_ERROR, "End of stream");
				disconnect();
			} else {
				bufin.len += sz;
			}
		}
	}
}

int main(int argc, char *argv[])
{
	int ch;
	struct sockaddr_in in;

	if (argc == 3)
		connect_to(argv[1], argv[2]);
	else if (argc > 1)
		line_put(LINE_INFO, "Ignoring command line arguments");

	input_init(&in_primary, "");
	input_init(&in_command, "(netline) ");
	input = &in_primary;

	buf_reset(&bufin);
	buf_reset(&bufout);

	line_put(LINE_INFO, "Welcome to netline v1.0");
	line_put(LINE_INFO, "Copyright (C) 2013 Alex Iadicicco");

	if (init_curses() < 0)
		return 1;

	evloop();
	disconnect();

	deinit_curses();
}
