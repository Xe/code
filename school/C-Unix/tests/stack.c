#include <stdio.h>

void stack(int x) {
  if (x == 0) return;
  stack(x - 1);
  printf("level %d: x is at %p\n", x, (void*)&x);
}

int main(void) {
  stack(4);
  return 0;
}
