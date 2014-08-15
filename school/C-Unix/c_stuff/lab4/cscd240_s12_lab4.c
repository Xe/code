#include <stdio.h>
int main()
{
int arr[] = { 2, 4, 6, 8, 10, 12, 14, 16, 18, 20};
int *aptr = arr;

/* This gives us an idea of the memory map */
printf("arr %p\n", arr);
printf("aptr %p\n", aptr);
printf("arr[1] %p\n", &arr[1]);
printf("arr[2] %p\n", &arr[2]);
printf("&aptr %p\n", &aptr);
/* end memory map */
++aptr;
printf("*aptr %i\n", *aptr);
printf("aptr %p\n", aptr);
// Command being executed is *aptr++
printf("*aptr++ %i\n", *aptr++);
printf("aptr %p\n", aptr);
// // Command being executed is *++aptr
printf("*++aptr %i\n", *++aptr);
printf("aptr %p\n", aptr);
*aptr += 1;
printf("*aptr %i\n", *aptr);
printf("aptr %p\n", aptr);
// // Command being executed is *(aptr+1)
printf("*(aptr+1) %i\n", *(aptr+1));
*(arr+2) = *aptr+100;
printf("*(arr+2) %i\n", *(arr+2));
//

aptr = arr + 5;
printf("*aptr %i\n", *aptr);
printf("aptr %p\n", aptr);
*(arr+2) =*aptr +5;
printf("*arr[2] %i\n", arr[2]);
aptr = (arr + 10);
printf("aptr %p\n", aptr);
printf("*aptr %i\n", *aptr);
//*aptr + 4 = *arr+2;

return 0;
}

