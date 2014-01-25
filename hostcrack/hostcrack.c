/* Bits of charybdis hacked up to uncloak hosts, letters only.
 * 
 * $ make uncloak CFLAGS="-std=c99 -O3"
 *
 * For 6 cloaked letters it's pretty quick (all taken on a Core i7-2677M at 1.80
 * GHz):
 *
 * $ ./cloak foobar.foobar    
 * fvonfn.foobar
 * $ time ./uncloak fvonfn.foobar
 * foobar.foobar
 * gqxyuf.foobar
 * larmre.foobar
 * picdzu.foobar
 * ./uncloak fvonfn.foobar  16.91s user 0.00s system 99% cpu 16.945 total
 *
 * Make that 7 and it's slower (roughly 26 times funnily enough with some
 * overhead of one extra character I guess), but not unfeasibly so:
 *
 * $ time ./uncloak lhmjxsg.foobar
 * foobarx.foobar
 * rhaebor.foobar
 * rmkyufu.foobar
 * ./uncloak lhmjxsg.foobar  487.48s user 0.00s system 99% cpu 8:08.37 total
 *
 * (Collisions seem quite common too.)
 *
 * With some more effort it would be possible to parallelize this, maybe even
 * run it on a GPU or something.
 *
 */
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Magic value for FNV hash functions */
#define FNV1_32_INIT 0x811c9dc5UL

uint32_t
fnv_hash(const unsigned char *s, int bits)
{
        uint32_t h = FNV1_32_INIT;

        while (*s)
        {
                h ^= *s++;
                h += (h<<1) + (h<<4) + (h<<7) + (h << 8) + (h << 24);
        }
        if (bits < 32)
                h = ((h >> bits) ^ h) & ((1<<bits)-1);
        return h;
}


void
do_host_cloak_host(const char *inbuf, char *outbuf)
{
        char b26_alphabet[] = "abcdefghijklmnopqrstuvwxyz";
        char *tptr;
        uint32_t accum = fnv_hash((const unsigned char*) inbuf, 32);

        strcpy(outbuf, inbuf);

        /* pass 1: scramble first section of hostname using base26 
         * alphabet toasted against the FNV hash of the string.
         *
         * numbers are not changed at this time, only letters.
         */
        for (tptr = outbuf; *tptr != '\0'; tptr++)
        {
                if (*tptr == '.')
                        break;

                if (isdigit(*tptr) || *tptr == '-')
                        continue;

                *tptr = b26_alphabet[(*tptr + accum) % 26];

                /* Rotate one bit to avoid all digits being turned odd or even */
                accum = (accum << 1) | (accum >> 31);
        }

        /* pass 2: scramble each number in the address */
        for (tptr = outbuf; *tptr != '\0'; tptr++)
        {
                if (isdigit(*tptr))
                        *tptr = '0' + (*tptr + accum) % 10;

                accum = (accum << 1) | (accum >> 31);
        }
}

int
main(int argc, char *argv[])
{
        if(argc < 2) {
                fprintf(stderr, "Usage: %s host\n", argv[0]);
                exit(1);
        }

        char *find = argv[1];
        char out[strlen(find) + 1];
        char in[strlen(find) + 1];

        strcpy(in, find);

        char *pad = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        char *end = strchr(in, '.') - 1;
        strncpy(in, pad, end - in);
        char *i;

        size_t c = 0;
        while(1) {
                do_host_cloak_host(in, out);
                if(!strcmp(out, find)) {
                        c++;
                        printf("%s\n", in);
                }
                i = end;
                while(i >= in) {
                        ++*i;
                        if(*i > 'z') {
                                *i = 'a';
                                i--;
                        } else {
                                break;
                        }
                }

                if(i < in) {
                        if(!c)
                                printf("not found\n");
                        exit(!c);
                }
        }
}
