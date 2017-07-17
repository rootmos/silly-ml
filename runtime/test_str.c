#include <runtime.h>
#include "test.h"

void test_itos() {
    const char* str = itos(123);
    assert(strlen(str) == 3);
    write(stdout, str, 3);
    write_newline(stdout);
}

void test_strlen() {
    assert(strlen("test") == 4);
    assert(strlen("") == 0);
}

void _start() {
    test_strlen();
    test_itos();
    exit(0);
}
