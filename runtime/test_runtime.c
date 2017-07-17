#include <runtime.h>
#include "test.h"

int main() {
    assert(getenv("") == NULL);
    assert(getenv("not-a-variable") == NULL);
    assert(strcmp("foo", getenv("TEST_VAR")) == 0);
    return 0;
}
