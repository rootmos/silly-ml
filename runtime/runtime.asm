.data
_env:
    .quad 0

.text

.global _start
_start:
    # save location of environment
    popq %rax
    inc %rax
    imulq $8, %rax
    mov %rsp, %rbx
    addq %rax, %rbx
    movq %rbx, _env

    call setup_heap
    call seed_xorshiftplus
    call main
    movq %rax, %rdi
    call exit

.global getenv
getenv:
    movq (_env), %r8  # %r8 := pointer to env var under consideration

getenv_loop:
    movq (%r8), %r9 # %r9 := env var under consideration
    testq %r9, %r9
    jz getenv_not_found

    movq %rdi, %r10   # %r10 := desired var

getenv_cmp_loop:
    movb (%r9), %al
    movb (%r10), %bl

    cmp %al, %bl
    je getenv_cmp_loop_next_char

    cmp $61, %al # '='
    jne getenv_next_var

    cmp $0, %bl
    jne getenv_next_var

    inc %r9
    movq %r9, %rax
    ret

getenv_cmp_loop_next_char:
    inc %r9
    inc %r10
    jmp getenv_cmp_loop

getenv_next_var:
    addq $8, %r8
    jmp getenv_loop

getenv_not_found:
    movq $0, %rax
    ret
