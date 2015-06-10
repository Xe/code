#include <string.h>
#include <stdint.h>

int32_t
buf_to_int32(char *buf)
{
    int32_t x;
    memcpy(&x, buf, sizeof(x));
    return x;
}

void
int32_to_buf(char *buf, int32_t x)
{
    memcpy(buf, &x, sizeof(x));
    return;
}


uint16_t
buf_to_uint16(char *buf)
{
    uint16_t x;
    memcpy(&x, buf, sizeof(x));
    return x;
}

void
uint16_to_buf(char *buf, uint16_t x)
{
    memcpy(buf, &x, sizeof(x));
    return;
}
