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

void test_strcmp() {
    assert(strcmp("", "") == 0);
    assert(strcmp("a", "a") == 0);
    assert(strcmp("a", "b") == -1);
    assert(strcmp("c", "b") == 1);
    assert(strcmp("a", "ab") == -1);
    assert(strcmp("ab", "a") == 1);
}

int main() {
    test_strlen();
    test_itos();
    test_strcmp();
    return 0;
}
