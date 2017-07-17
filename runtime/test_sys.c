#include <runtime.h>
#include "test.h"

int main() {
    assert(write(stdout, "I'm a test write!\n", 18) == 18);
    return 0;
}
