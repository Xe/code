#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main() {
  if (setuid(0)) {
    perror("suid");
    exit(1);
  }

  execle("/usr/bin/systemctl", "systemctl", "restart", "nginx", NULL, NULL);
  return 0;
 }
