#ifndef sys_h
#define sys_h

#include <types.h>

void exit(int) __attribute__ ((noreturn));
ssize_t write(int fd, const void* buf, size_t size);
void write_newline(int fd);

#define stdin  0
#define stdout 1
#define stderr 2

#endif
