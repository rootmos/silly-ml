.data

itos_buffer:
    .space 20
itos_buffer_end:
    .byte 0

.text

# input:
#   %rdi - integer to stringlify
# output:
#   %rax - null-terminated string of digits
#          (will be overwritten next call to itos)
#   %rbx - number of digits (length of string)
.global itos
itos:
    movq %rdi, %rax
    movq $itos_buffer_end, %rcx # buffer
    movq $0, %rbx               # number of digits
    movq $10, %r8
itos_loop:
    movq $0, %rdx
    divq %r8
    incq %rbx           # increment digits
    addb $0x30, %dl     # convert to ascii
    decq %rcx           # write the remainder to buffer
    movb %dl, (%rcx)
    test %rax, %rax     # if not zero
    jnz itos_loop       # jump back to loop
itos_done:
    movq %rcx, %rax
    ret

.global strlen
strlen:
    movq $0, %rax
strlen_loop:
    movb (%rdi), %bl
    test %bl, %bl
    jz strlen_done
    inc %rax
    inc %rdi
    jmp strlen_loop
strlen_done:
    ret

.global strcmp
strcmp:

strcmp_loop:
    movb (%rdi), %al
    movb (%rsi), %bl

    cmp %al, %bl
    jg strcmp_first
    jl strcmp_second

    test %al, %al
    jz strcmp_equal

    inc %rdi
    inc %rsi
    jmp strcmp_loop

strcmp_first:
    movq $-1, %rax
    ret

strcmp_second:
    movq $1, %rax
    ret

strcmp_equal:
    movq $0, %rax
    ret
