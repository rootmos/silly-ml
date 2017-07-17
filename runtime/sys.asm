.text

.global exit
exit:
    movq $60, %rax
    syscall


# input:
#   %rdi - fd
#   %rsi - buf
#   %rdx - size
# output:
#   %rax - bytes written
.global write
write:
    movq $0x1, %rax
    syscall
    ret

# input:
#   %rdi - fd
.global write_newline
write_newline:
    push $0x0a
    movq %rsp, %rsi
    movq $1, %rdx
    call write
    add $8, %rsp
    ret
