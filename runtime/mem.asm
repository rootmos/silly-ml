.data

heap_chunk_size:
    .quad 409600

root_chunk:
    .quad 0

defragment_countdown:
    .quad 0


.text

# chunk
# qw = next chunk | qw = end chunk | -- slots

# slot
# 1b flags |  -- 7b size -- | data --
# flags: 0b------mf
# f = free

#   - ZF=0 if \flags free
#   - ZF=1 if \flags used
.macro is_free flags
    testb $0b1, \flags
.endm

.macro set_free flags
    orb $0b1, \flags
.endm

.macro unset_free flags
    andb $0b11111110, \flags
.endm

#   - ZF=0 if \flags marked
#   - ZF=1 if \flags unmarked
.macro is_marked flags
    testb $0b10, \flags
.endm

.macro set_mark flags
    orb $0b10, \flags
.endm

.macro unset_mark flags
    andb $0b11111101, \flags
.endm

.macro slot_size slot dest
    movq (\slot), \dest
    shlq $8, \dest
    shrq $8, \dest
.endm

# input
#   %rax - bytes to request
new_pages:
    movq %rax, %rsi       # len
    movq $9, %rax         # mmap
    movq $0, %rdi         # addr
    movq $0b11, %rdx      # prot: PROT_WRITE (0b10) | PROT_READ (0b01)
    movq $0b100010, %r10  # flags: MAP_PRIVATE (0b10) | MAP_ANONYMOUS (0b100000)
    movq $-1, %r8         # fd
    movq $0, %r9          # off
    syscall
    ret

# output
#   %rax - addr of chunk
# (uses: %rax, %rbx, %r10)
new_chunk:
    movq heap_chunk_size, %rax
    call new_pages

    movq $0, (%rax) # first quad: next chunk

    mov %rax, 8(%rax)
    movq heap_chunk_size, %r10 # second quad: end of chunk
    addq %r10, 8(%rax)

    # initialize first slot
    movq heap_chunk_size, %rbx # set size
    sub $16, %rbx
    movq %rbx, 16(%rax)
    # set flags
    set_free 23(%rax)
    unset_mark 23(%rax)

    ret

.global setup_heap
setup_heap:
    call new_chunk
    movq %rax, root_chunk
    ret

.macro  round dst bits
.rept \bits
shrq $1, \dst
adcq $0, \dst
.endr
shlq $3, \dst
.endm

# input:
#  %rdi - size
# output:
#  %rax - allocated memory
.global malloc
malloc:
    movq %rdi, %rax
    round %rax 3
    addq $8, %rax # add slot header size to required size

    movq root_chunk, %r10 # %r10 := current chunk

malloc_start:
    movq 8(%r10), %rcx    # %rcx := end of chunk
    movq %r10, %rbx       # %rbx := slot under consideration
    addq $16, %rbx

malloc_loop:
    slot_size %rbx %r8    # %r8 := size of current slot

    is_free 7(%rbx)
    jnz malloc_found_free_slot

malloc_move_to_next_slot:
    addq %r8, %rbx

    cmpq %rcx, %rbx
    je malloc_next_chunk

    jmp malloc_loop

malloc_next_chunk:
    movq (%r10), %r11     # %r11 - next chunk

    cmpq $0, %r11         # if next chunk == NULL
    je malloc_add_chunk
    movq %r11, %r10       # else set next as current chunk
    jmp malloc_start

malloc_add_chunk:
    pushq %rax
    pushq %rbx
    pushq %r10
    call new_chunk
    popq %r10
    movq %rax, (%r10)
    popq %rbx
    popq %rax
    mov (%r10), %r10
    jmp malloc_start

malloc_found_free_slot:
    cmpq %rax, %r8
    jl malloc_move_to_next_slot

malloc_found_suitable_slot:
    movq %rbx, %r9     # %r9 := addr of chosen slot
    unset_free 7(%rbx) # unset free flag

    movq %r8, %r13     # %r13 := the size of new next slot
    subq %rax, %r13

    cmpq $8, %r13     # if next slot would be too small
    jle malloc_done   # then don't partinion current slot

    # else partition slot
    movq %rax, (%rbx) # set size
    addq %rax, %rbx   # move %rbx to the new next slot

    cmpq %rcx, %rbx   # check if its at the edge of the chunk
    je malloc_done

    movq %r13, (%rbx) # set new slot's size
    set_free 7(%rbx)  # set its free frag

malloc_done:
    movq %r9, %rax
    addq $8, %rax
    ret

# input:
#  %rdi - allocated memory to free
.global free
free:
    set_free -1(%rdi)  # set its free frag

    movq defragment_countdown, %rax
    testq %rax, %rax
    jnz free_done

    call defragment
    movq $50, defragment_countdown

free_done:
    decq defragment_countdown
    ret


defragment:
    movq root_chunk, %r10 # %r10 := current chunk

defragment_start:
    movq %r10, %rbx
    addq $16, %rbx        # %rbx := current slot
    movq 8(%r10), %rcx    # %rcx := end of chunk

defragment_consider_slot:
    cmpq %rcx, %rbx   # check if its at the edge of the chunk
    je defragment_chunk_done

    slot_size %rbx, %r8   # %r8 := current slot size

    is_free 7(%rbx)
    jz defragment_move_to_next_slot

    mov %rbx, %r12
    addq %r8, %r12

    cmpq %rcx, %r12   # check if its at the edge of the chunk
    je defragment_chunk_done

    is_free 7(%r12)
    jz defragment_move_to_next_slot

    slot_size %r12 %r13
    addq %r13, (%rbx)
    addq %r13, %r8

defragment_move_to_next_slot:
    addq %r8, %rbx
    jmp defragment_consider_slot

defragment_chunk_done:
    movq (%r10), %r11     # %r11 - next chunk

    cmpq $0, %r11         # if next chunk == NULL
    je defragment_done
    movq %r11, %r10       # else set next as current chunk
    jmp defragment_start

defragment_done:
    ret
