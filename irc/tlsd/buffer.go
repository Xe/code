package main

/*
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

int32_t buf_to_int32(char *buf);
void int32_to_buf(char *buf, int32_t x);
int16_t buf_to_int16(char *buf);
void int16_to_buf(char *buf, int16_t x);
*/
import "C"

import (
	"unsafe"
)

func bufToInt32(buf string) int {
	cstr := C.CString(buf)
	defer C.free(unsafe.Pointer(cstr))

	res := C.buf_to_int32(cstr)

	return int(res)
}
