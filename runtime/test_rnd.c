#include <runtime.h>

/* Reference implementation taken from:
 * https://en.wikipedia.org/wiki/Xorshift#xorshift.2B
 */

uint64_t s[2] = { 0x0000000000000009, 0x0000000000000011 };

uint64_t reference_xorshift128plus(void) {
    uint64_t x = s[0];
    uint64_t const y = s[1];
    s[0] = y;
    x ^= x << 23; // a
    s[1] = x ^ y ^ (x >> 17) ^ (y >> 26); // b, c
    return s[1] + y;
}

int main() {
    set_seed_xorshiftplus(s[0], s[1]);
    if (xorshiftplus() == reference_xorshift128plus()) {
        return 0;
    } else {
        return 1;
    }
}
