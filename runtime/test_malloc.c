#include <runtime.h>

#define MAX_DATA_SIZE 1000
#define MAX_SLICES 10000

struct slice {
    uint64_t alive;
    uint64_t size;
    uint64_t data[MAX_DATA_SIZE];
    uint64_t* mem;
};

struct slice slice[MAX_SLICES];

void initialize_slices() {
    for (uint64_t i = 0; i < MAX_SLICES; ++i) {
        slice[i].alive = 0;
    }
}

void allocate_slice(uint64_t i) {
    slice[i].alive = 1;
    slice[i].size = xorshiftplus() % MAX_DATA_SIZE;
    slice[i].mem = (uint64_t*)malloc(slice[i].size * sizeof(uint64_t));

    for (uint64_t j = 0; j < slice[i].size; ++j) {
        uint64_t d = xorshiftplus();
        slice[i].mem[j] = d;
        slice[i].data[j] = d;
    }
}

void free_slice(uint64_t i) {
    slice[i].alive = 0;
    free(slice[i].mem);
}

void take_action(uint64_t i) {
    if (xorshiftplus() % 4 == 0) {
        if (slice[i].alive) {
            free_slice(i);
        } else {
            allocate_slice(i);
        }
    }
}

void verify(uint64_t i) {
    if (slice[i].alive) {
        for (uint64_t j = 0; j < slice[i].size; ++j) {
            if (slice[i].mem[j] != slice[i].data[j]) {
                exit(1);
            }
        }
    }
}

void run(uint64_t n) {
    for (uint64_t i = 0; i < MAX_SLICES; ++i) {
        take_action(i);
    }

    for (uint64_t i = 0; i < MAX_SLICES; ++i) {
        verify(i);
    }
}


void _start() {
    setup_heap();
    seed_xorshiftplus();

    initialize_slices();
    for (uint64_t n = 0; n < 100; ++n) {
        run(n);
    }

    exit(0);
}
