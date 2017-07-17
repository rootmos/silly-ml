#include <runtime.h>

void _start() {
    const char* str = itos(123);

    write(stdout, str, 3);
    
    write_newline(stdout);

    exit(0);
}
