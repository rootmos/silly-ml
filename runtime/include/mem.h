#ifndef mem_h
#define mem_h

#include <types.h>

void* malloc(size_t size);
void free(const void* ptr);

void setup_heap();

#endif
