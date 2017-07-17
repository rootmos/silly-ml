#ifndef rnd_h
#define rnd_h

#include <types.h>

void seed_xorshiftplus();
void set_seed_xorshiftplus(uint64_t s0, uint64_t s1);

uint64_t xorshiftplus();

#endif
