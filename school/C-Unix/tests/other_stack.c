#include <stdio.h>

int f(int *x)
{
  int a;
  return x == NULL ? f(&a) : &a - x;
}

int main(void)
{
  printf("stack grows %s!\n", f(NULL) < 0 ? "down" : "up");
  return 0;
}
