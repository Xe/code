#include <stdio.h>

//stolen from http://countercomplex.blogspot.com/2011/10/algorithmic-symphonies-from-one-line-of.html

main(t,v){for(t=0;;t++)putchar(v=(v>>1)+(v>>4)+t*(((t>>16)|(t>>6))&(69&(t>>9))));}

