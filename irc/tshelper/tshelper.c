/* tshelper - A telnet server helper script for IRC server and services
**            developers
**
** Copyright 2004, W. Campbell <wcampbel@botbay.net>
**
** $Id: tshelper.c,v 1.32 2004/05/18 02:48:40 wcampbel Exp $
*/

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdarg.h>
#include <time.h>
#include <locale.h>
#include <sys/un.h>

#define VERSION		"0.7"
#define BUFSIZE		512

#define FLAG_PING	0x00000001
#define FLAG_LOG	0x00000002
#define FLAG_NOPREF	0x00000004
#define FLAG_NOECHO	0x00000008
#define FLAG_UNIX	0x00000010

/* this will make our code for checking for -u
** cleaner.
*/
#define IsUnix()	(flags & FLAG_UNIX)

#define SHOW_USER	1
#define SHOW_SCRIPT	2
#define SHOW_HIDE	3

/* Ugly globals, these will be in a struct someday */
static char *progname;
static int max_fds = 0;
static int servsock = -1;
static char stdin_backing[BUFSIZE + 1];
static char servsock_backing[BUFSIZE + 1];
static unsigned int flags;
FILE *logfile;

static struct addrinfo * gethostinfo(char const *, int);
static int create_server_socket(char *, int, char *);
static int create_unix_socket(char *);
static void syntax(int);
static char sock_getbyte(int);
static void sock_process_line(int, char *);
static void parse_server(char *);
static void parse_stdin(char *);
static void send_to_server(int, char *, ...);
static char *local_ctime(time_t);
static void run_script_file(char *);

int main(int argc, char *argv[])
{
  int port = 0;
  char *server = NULL;
  char *vhost = NULL;
  char *script = NULL;
  fd_set readfds, nullfds;
  struct timeval timeout;
  int result;
  char c;

  progname = argv[0];
  (void) setlocale(LC_TIME, "");

  while ( (c = getopt(argc, argv, "hpul:H:vs:ne")) != -1)
  {
    switch (c)
    {
      case 'h':
      case '?': /* getopt errors are sent to the ? character */
        syntax(0);
        /* NOT REACHED */
        break;
      case 'p':
        flags |= FLAG_PING;
        break;
      case 'l':
        flags |= FLAG_LOG;
        logfile = fopen(optarg, "a");
        if (logfile == NULL)
        {
          fprintf(stderr, "%s: Logfile error: %s\n", progname, strerror(errno));
          exit(9);
        }
        fprintf(logfile, "*** tshelper log file opened:  %s\n",
                local_ctime(time(NULL)));
        break;
      case 'H':
        vhost = optarg;
        break;
      case 'n':
        flags |= FLAG_NOPREF;
        break;
      case 'e':
        flags |= FLAG_NOECHO;
        break;
      case 'u':
	flags |= FLAG_UNIX;
	break;
      case 'v':
        fprintf(stderr, "tshelper(%s) (c) 2004 W. Campbell, J. Johnston\n",
                VERSION);
        exit(0);
        /* NOT REACHED */
        break;
      case 's':
        script = optarg;
        break;
    }
  }
      
  argc -= optind;
  argv += optind;
  if (argc < 2 && !IsUnix())
    syntax(1);

  server = argv[0];

  if (IsUnix())
    port = 0;
  else
    port = atoi(argv[1]);

  /* Greet the user, provided we don't use a script */
  if (script == NULL)
  {
    printf("*** tshelper - telnet server helper for ircd/services developers\n");
    printf("*** tshelper(%s) (c) 2004 W. Campbell, J. Johnston\n", VERSION);
    printf("*** Control Codes:  ^K - Insert the current timestamp\n");
    printf("***                 ^E - Insert a literal blank line\n");
  }

  /* Create the socket to connect to the server */
  if (IsUnix()) /* Are we connecting a UNIX socket? */
	servsock = create_unix_socket(server);
  else
  	servsock = create_server_socket(server, port, vhost);

  FD_ZERO(&nullfds);
  memset(stdin_backing, '\0', sizeof(stdin_backing));
  memset(servsock_backing, '\0', sizeof(servsock_backing));

  /* Set stdin to be line buffered */
  setlinebuf(stdin);

  if (script != NULL)
    run_script_file(script);

  /* Enter I/O loop */
  while (1)
  {
    FD_ZERO(&readfds);
    timeout.tv_sec = 1; /* Longer for this? */
    timeout.tv_usec = 0L;

    FD_SET(servsock, &readfds);
    FD_SET(STDIN_FILENO, &readfds);

    result = select(max_fds + 1, &readfds, &nullfds, &nullfds, &timeout);
    if (result > 0)
    {
      if (FD_ISSET(servsock, &readfds))
        sock_process_line(servsock, servsock_backing);
      if (FD_ISSET(STDIN_FILENO, &readfds))
        sock_process_line(STDIN_FILENO, stdin_backing);
    }
    else if (result == 0)
    {
      /* select timed out, if we set a higher time, maybe we want to
      ** ping the uplink?
      */
    }
    else
    {
      /* select returned an error of some kind */
      if (errno != EINTR) /* EINTR is ok, anything else we need to die on */
      {
        fprintf(stderr, "%s: Connection error: %s\n", progname,
                strerror(errno));
        close(servsock);
        if (flags & FLAG_LOG)
          fclose(logfile);
        exit(4);
      }
    }
  }

  return 0;
}

static void syntax(int exitcode)
{
  fprintf(stderr, "Syntax: %s [-h|-?] [-p] [-l logfile] [-H vhost] [-v]\n"
                  "         [-s scriptfile] [-n] [-e] [-u] <hostname/ip> "
                  "<port>\n", progname);
  fprintf(stderr, "-h Show this help text.\n");
  fprintf(stderr, "-p Do not parse PING, force the user to manage it.\n");
  fprintf(stderr, "-l Log all text to the given logfile.\n");
  fprintf(stderr, "-H Connect using the supplied vhost (bind ip).\n");
  fprintf(stderr, "-v Show the version and exit.\n");
  fprintf(stderr, "-s Use the given script file after connection to the server.\n");
  fprintf(stderr, "-n Use no prefixes (<== ==> S=>).\n");
  fprintf(stderr, "-e Disable local echo.\n");
  fprintf(stderr, "-u Interpret <hostname/ip> as a UNIX socket\n   Note: You do not need to specify port when using -u\n");
  exit(exitcode);
}

static int create_server_socket(char *server, int port, char *vhost)
{
  struct addrinfo *hostres, *res;
  struct addrinfo *bindres = NULL;
  int sock = -1;
  int optval;

  hostres = gethostinfo(server, port);
  if (hostres == NULL)
  {
    fprintf(stderr, "%s: Lookup failure for hostname %s\n", progname, server);
    if (flags & FLAG_LOG)
      fclose(logfile);
    exit(2);
  }

  if (vhost != NULL)
  {
    bindres = gethostinfo(vhost, 0); /* port 0 makes the OS select */
    if (bindres == NULL)
    {
      fprintf(stderr, "%s: Lookup failure for vhost %s\n", progname, vhost);
      if (flags & FLAG_LOG)
        fclose(logfile);
      exit(10);
    }
  }

  for (res = hostres; res != NULL; res = res->ai_next)
  {
    sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sock < 0) /* Error, move on to the next entry returned by DNS */
      continue;

    if (sock > max_fds)
      max_fds = sock;

    optval = 1;
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)optval, sizeof(optval));

    if (bindres != NULL)
    {
      if (bind(sock, bindres->ai_addr, bindres->ai_addrlen) < 0)
      {
        fprintf(stderr, "%s: Bind error for vhost %s\n", progname, vhost);
        if (flags & FLAG_LOG)
          fclose(logfile);
        exit(11);
      }
    }

    if ((connect(sock, res->ai_addr, res->ai_addrlen)) != 0)
    {
      fprintf(stderr, "%s: Connect error: %s\n", progname, strerror(errno));
      sock = -1;
    }
    else
    {
      printf("*** Connected to %s %d\n", server, port);
      break;
    }
  }

  freeaddrinfo(hostres);
  if (bindres != NULL)
    freeaddrinfo(bindres);

  if (sock == -1)
  {
    fprintf(stderr, "%s: Unable to connect to %s %d, exiting\n", progname,
            server, port);
    if (flags & FLAG_LOG)
      fclose(logfile);
    exit(3);
  }

  return sock;
}

static int create_unix_socket(char *server)
{
  int sock;
  struct sockaddr_un UNIXaddr;

  sock = socket(AF_UNIX, SOCK_STREAM, 0);

  if (sock < 0)
  {
    fprintf(stderr, "%s: Unable to create socket: %s, exiting\n", progname,
            strerror(errno));
    exit(14);
  }

  memset((char*)&UNIXaddr, 0, sizeof(UNIXaddr));
  UNIXaddr.sun_family = AF_UNIX;
  strcpy(UNIXaddr.sun_path, server);

  if (connect(sock, (struct sockaddr *)&UNIXaddr, SUN_LEN(&UNIXaddr)) < 0)
  {
    fprintf(stderr, "%s: Unable to connect to %s: %s, exiting\n", progname,
            server, strerror(errno));
    exit(3);
  }

  if (sock > max_fds)
    max_fds = sock;

  return sock;
}

/* Stolen from FreeBSD's whois client, modified for Sentinel by W. Campbell */
static struct addrinfo * gethostinfo(char const *host, int port)
{
  struct addrinfo hints, *res;
  int error;
  char portbuf[6];

  memset(&hints, 0, sizeof(hints));
  hints.ai_flags = 0;
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  snprintf(portbuf, 6, "%d", port);
  error = getaddrinfo(host, portbuf, &hints, &res);
  if (error)
  {
    fprintf(stderr, "%s: gethostinfo error: %s: %s\n", progname, host,
            gai_strerror(error));
    return (NULL);
  }
  return (res);
}

static char sock_getbyte(int fd)
{
  static char message[BUFSIZE + 1];
  static char *c = message;
  char rc;
  int n;
  static int data_in_buffer = 0;

  if (data_in_buffer == 0)
  {
    n = read(fd, message, BUFSIZE);
    if (n <= 0)
    {
      if (fd == servsock)
      {
        if (n == 0)
          fprintf(stderr, "%s: EOF from server, exiting\n", progname);
        else
          fprintf(stderr, "%s: Lost connection to server: %s\n", progname,
                  strerror(errno));
        close(servsock);
        if (flags & FLAG_LOG)
          fclose(logfile);
        if (n == 0)
          exit(13);
        else
          exit(5);
      }
      if ((fd == STDIN_FILENO) && (n == 0))
      {
        /* EOF received from STDIN, gracefully exit */
        printf("*** EOF from user, exiting\n");
        close(servsock);
        if (flags & FLAG_LOG)
          fclose(logfile);
        exit(0);
      }
      else
      {
        /* How can you get a read error from stdin? */
        fprintf(stderr, "%s: Unknown read error: %s\n", progname,
                strerror(errno));
        close(servsock);
        if (flags & FLAG_LOG)
          fclose(logfile);
        exit(6);
      }
    }
    message[n] = '\0';
    c = message;
    data_in_buffer = 1;
  }
  if (*c == '\0')
  {
    data_in_buffer = 0;
    message[0] = '\0';
    return *c;
  }
  rc = *c;
  c++;
  return rc;
}

static void sock_process_line(int fd, char *backing)
{
  char message[BUFSIZE + 1];
  int i = 0;
  char c;

  if (backing[0] != '\0')
  {
    /* Oh how I wish everything used strlcpy() */
    strncpy(message, backing, BUFSIZE);
    message[BUFSIZE] = '\0';
    i = strlen(message);
    message[i] = '\0'; /* needed? */
    backing[0] = '\0';
  }

  while (1)
  {
    c = sock_getbyte(fd);
    if (c == '\0') /* End of packet */
    {
      if (i > 0)
      {
        message[i] = '\0';
        strncpy(backing, message, BUFSIZE);
        backing[BUFSIZE] = '\0';
      }
      break;
    }
    if ((c == '\r') || (c == '\n'))
    {
      if (i > 0) /* Filter out blank lines */
      {
        message[i] = '\0';
        if (fd == servsock)
          parse_server(message);
        else
          parse_stdin(message);
        i = 0;
      }
      continue;
    }
    /* Any other character, add to the message */
    message[i] = c;
    i++;
    /* sock_getbyte() will only read in the same number of characters, so
    ** overflows aren't possible, on IRC that is, it's possible via stdin.
    */
    if (i > BUFSIZE) /* The highest i allowed is BUFSIZE, message[BUFSIZE] */
    {
      fprintf(stderr, "%s: Read error, line too long\n", progname);
      close(servsock);
      if (flags & FLAG_LOG)
        fclose(logfile);
      exit(7);
    }
  }
}

static void send_to_server(int showtype, char *fmt, ...)
{
  va_list ap;
  char buf[BUFSIZE + 1];
  char tmpbuf[BUFSIZE + 1];
  int len, n;
  int i, j;

  va_start(ap, fmt);
  vsnprintf(tmpbuf, BUFSIZE + 1, fmt, ap);
  len = strlen(tmpbuf);

  for (i = j = 0; i < len; i++)
  {
    if (j == BUFSIZE)
    {
      fprintf(stderr, "%s: Read error, line too long\n", progname);
      close(servsock);
      if (flags & FLAG_LOG)
        fclose(logfile);
      exit(7);
    }

    if (tmpbuf[i] == '\013') /* CTRL-K */
    {
      time_t ts;
      char timestr[11];

      buf[j] = '\0';
      ts = time(NULL);
      snprintf(timestr, 11, "%ld", (long)ts);
      strncat(buf, timestr, BUFSIZE);
      buf[BUFSIZE] = '\0';
      j = strlen(buf);
      continue;
    }

    if (tmpbuf[i] == '\005') /* CTRL-E */
    {
      /* Explicit blank line */
      continue;
    }

    /* Any other character is handled literally */
    buf[j] = tmpbuf[i];
    j++;
  }

  /* When complete, make sure it's terminated */
  buf[j] = '\0';
  len = j;

  if (len == (BUFSIZE - 2))
  {
    fprintf(stderr, "%s: Read error, line too long\n", progname);
    close(servsock);
    if (flags & FLAG_LOG)
      fclose(logfile);
    exit(7);
  }

  if ((showtype != SHOW_HIDE) && !(flags & FLAG_NOECHO))
  {
    char *prefix;

    if (flags & FLAG_NOPREF)
    {
      prefix = "";
    }
    else
    {
      if (showtype == SHOW_USER)
        prefix = "==> ";
      else
        prefix = "S=> ";
    }

    printf("%s%s\n", prefix, buf);
    if (flags & FLAG_LOG)
      fprintf(logfile, "%s%s\n", prefix, buf);
  }

  /* After it's printed in UNIX fasion, add the IRC required \r\n */
  strcat(buf, "\r\n"); /* Will always be safe */
  buf[BUFSIZE] = '\0';
  len += 2;

  n = write(servsock, buf, len);
  if (n == -1)
  {
    fprintf(stderr, "%s: Write error to server: %s\n", progname,
            strerror(errno));
    close(servsock);
    if (flags & FLAG_LOG)
      fclose(logfile);
    exit(8);
  }

  va_end(ap);
}

static void parse_server(char *message)
{
  char *cmd;
  char *prefix;

  if (message[0] == '\0')
    return;

  /* Slightly inefficient but who cares */
  if (flags & FLAG_NOPREF)
    prefix = "";
  else
    prefix = "<== ";

  /* Anything we want to handle, check for here */

  if (!(flags & FLAG_PING)) /* FLAG_PING makes tshelper NOT handle PING */
  {
    if (strncasecmp(message, "PING ", 5) == 0)
    {
      char *rest = message + 5; /* pointer arithmetic is ugly */
 
      send_to_server(SHOW_HIDE, "PONG %s", rest);
      return;
    }
  }

  cmd = strchr(message, ' ');
  if (cmd == NULL)
  {
    printf("%s%s\n", prefix, message);
    if (flags & FLAG_LOG)
      fprintf(logfile, "%s%s\n", prefix, message);
    return;
  }
  cmd++;

  if (*cmd != '\0')
  {
    if (!(flags & FLAG_PING))
    {
      if (strncasecmp(cmd, "PING ", 5) == 0)
      {
        /* :irc.XXXX.st PING irc.XXXX.st :jupe.YYYY.net */
        char *par1 = NULL, *par2 = NULL;
        par1 = cmd + 5;
        if (*par1 == '\0')
        {
          /* Error, show it to the user */
          printf("%s%s\n", prefix, message);
          if (flags & FLAG_LOG)
            fprintf(logfile, "%s%s\n", prefix, message);
          return;
        }
        par2 = strchr(par1, ' ');
        if (par2 == NULL)
        {
          /* Another bad PING */
          printf("%s%s\n", prefix, message);
          if (flags & FLAG_LOG)
            fprintf(logfile, "%s%s\n", prefix, message);
          return;
        }
        *par2 = '\0';
        par2++; /* Skip the space I just changed */
        if (*par2 == '\0')
        {
          /* Another error */
          printf("%s%s\n", prefix, message);
          if (flags & FLAG_LOG)
            fprintf(logfile, "%s%s\n", prefix, message);
          return;
        }
        if (*par2 == ':')
        {
          par2++; /* Skip the ':' */
          if (*par2 == '\0')
          {
            printf("%s%s\n", prefix, message);
            if (flags & FLAG_LOG)
              fprintf(logfile, "%s%s\n", prefix, message);
            return;
          }
        }
        /* FINALLY, we have a working set of parameters */
        send_to_server(SHOW_HIDE, ":%s PONG %s :%s", par2, par2, par1);
        return;
      }
    }
  }

  /* Any other text from the server, we just display to the user */
  printf("%s%s\n", prefix, message);
  if (flags & FLAG_LOG)
    fprintf(logfile, "%s%s\n", prefix, message);
}

static void parse_stdin(char *message)
{
  /* CTRL-K, insert the current TS, is handled by send_to_server() */
  send_to_server(SHOW_USER, "%s", message);
}

/* Pretty much taken straight from Sentinel */
static char *local_ctime(time_t ts)
{
  static char buf[32];
  struct tm *ltm = localtime(&ts);
  static char *gcc_sucks = "%c";

  /* %c is the POSIX localized replacement for ctime() */

  /* gcc complains about some non-BSD implementations not using
  ** a 4 digit year in some locales.  The warning can be ignored.
  **
  ** The lameness in this function is the only way I know of that
  ** the warning can be ignored.
  **
  ** Some versions of gcc support -Wno-y2k, but apparently it's
  ** only gcc3 and higher.  Using a custom -W parameter will cause
  ** issues when run with another compiler, so for now, this lameness
  ** stays.
  */
  strftime(buf, 32, gcc_sucks, ltm);
  return buf;
}

static void run_script_file(char *script)
{
  FILE *fp;
  char buf[BUFSIZE];
  char *ptr;

  fp = fopen(script, "r");
  if (fp == NULL)
  {
    fprintf(stderr, "%s: Script file error: %s", progname, strerror(errno));
    if (flags & FLAG_LOG)
      fclose(logfile);
    exit(12);
  }

  while (fgets(buf, BUFSIZE, fp) != NULL)
  {
    if (buf[0] == '#')
      continue; /* Comment check */

    ptr = strchr(buf, '\n');
    if (ptr != NULL)
    {
      if (ptr == buf)
        continue; /* This was a blank line in the script file, skip */
      *ptr = '\0';
      ptr--;
      if (*ptr == '\r')
        *ptr = '\0';
    }
    if (buf[0] == '\0')
      continue; /* Another blank line */

    /* We have a line to send to the server */
    send_to_server(SHOW_SCRIPT, "%s", buf);
  }

  fclose(fp);
}

