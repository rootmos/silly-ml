.data

xorshift_state:
    .quad 0
    .quad 0

.text

.global seed_xorshiftplus
seed_xorshiftplus:
    movq $318, %rax            # sys_getrandom
    movq $xorshift_state, %rdi # buf
    movq $16, %rsi             # size
    movq $0x2, %rdx            # flags := GRND_RANDOM (0x0002)
    syscall
    ret

.global set_seed_xorshiftplus
set_seed_xorshiftplus:
    movq %rdi, xorshift_state
    movq %rsi, xorshift_state + 8
    ret

.global xorshiftplus
xorshiftplus:
    movq xorshift_state, %rcx        # x := state[0]
    movq (xorshift_state + 8), %rbx  # y := state[1]
    movq %rbx, xorshift_state        # state[0] := y

    movq %rcx, %rax
    shl $23, %rax
    xorq %rax, %rcx    # x ^= x << 23

    movq %rbx, %rax
    shr $26, %rax      # (y >> 26)

    movq %rcx, %rdx
    shr $17, %rdx      # (x >> 17)

    xorq %rdx, %rax    # (x >> 17) ^ (y >> 26)
    xorq %rbx, %rax    # y ^ (x >> 17) ^ (y >> 26)
    xorq %rcx, %rax    # x ^ y ^ (x >> 17) ^ (y >> 26)

    # s[1] := x ^ y ^ (x >> 17) ^ (y >> 26)
    movq %rax, (xorshift_state + 8)

    addq %rbx, %rax # return s[1] + y
    ret
