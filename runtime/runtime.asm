.data

_env:
    .quad 0

.text

.global _start
_start:
    popq _env
    call setup_heap
    call seed_xorshiftplus
    call main
    movq %rax, %rdi
    call exit
