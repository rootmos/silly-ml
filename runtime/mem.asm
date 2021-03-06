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

    movq $0, (%rax)             # first quad: next chunk

    mov %rax, 8(%rax)           # second quad: end of chunk
    movq heap_chunk_size, %r10
    addq %r10, 8(%rax)

    mov %rax, 16(%rax)          # third quad: pointer to first slot
    add $24, 16(%rax)

    # initialize first slot
    movq heap_chunk_size, %rbx # set size
    sub $24, %rbx
    movq %rbx, 24(%rax)
    # set flags
    set_free 31(%rax)

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
    pushq %r8
    pushq %r9
    pushq %r10
    pushq %r11
    pushq %r13
    movq %rdi, %rax
    round %rax 3
    addq $8, %rax # add slot header size to required size
    movq root_chunk, %r10 # %r10 := current chunk

    # test whether:
    # (requested size (%rax) + chunk header) <= heap_chunk_size
    movq %rax, %rbx
    add $32, %rbx
malloc_heap_chunk_size_check:
    cmpq heap_chunk_size, %rbx
    jle malloc_start

    # if not, double heap_chunk_size
    shlq $1, heap_chunk_size
    jmp malloc_heap_chunk_size_check

malloc_start:
    movq 8(%r10), %rcx    # %rcx := end of chunk
    movq 16(%r10), %rbx   # %rbx := slot under consideration

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
    jle malloc_done   # then don't partition current slot

    # else partition slot
    movq %rax, (%rbx) # set size
    addq %rax, %rbx   # move %rbx to the new next slot

    cmpq %rcx, %rbx   # check if its at the edge of the chunk
    je malloc_done

    movq %r13, (%rbx) # set new slot's size
    set_free 7(%rbx)  # set its free flag
    unset_mark 7(%rbx)

malloc_done:
    movq %r9, 16(%r10)

    movq %r9, %rax
    addq $8, %rax
    popq %r13
    popq %r11
    popq %r10
    popq %r9
    popq %r8
    ret

# input:
#  %rdi - allocated memory to free
.global free
free:
    set_free -1(%rdi)  # set its free flag
    unset_mark -1(%rdi)  # unset its mark flag

    movq defragment_countdown, %rax
    testq %rax, %rax
    jnz free_done

    call defragment
    movq $300, defragment_countdown

free_done:
    decq defragment_countdown
    ret

# input:
#  %rdi - allocated memory to be marked
# output:
#  %rax - 1 if mark was set, 0 if already set
.global mark
mark:
    is_marked -1(%rdi)
    jz mark_do_set_mark
    movq $0, %rax
    ret
mark_do_set_mark:
    set_mark -1(%rdi)  # set its mark flag
    movq $1, %rax
    ret

defragment:
    movq root_chunk, %r10 # %r10 := current chunk

defragment_start:
    movq %r10, %rbx
    addq $24, %rbx        # %rbx := current slot (now: first slot)
    movq %rbx, 16(%r10)    # reset chunks slot pointer
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

.global sweep
sweep:
    movq root_chunk, %r10 # %r10 := current chunk

sweep_start:
    movq %r10, %rbx
    addq $24, %rbx        # %rbx := current slot
    movq %rbx, 16(%r10)   # reset chunks slot pointer
    movq 8(%r10), %rcx    # %rcx := end of chunk

sweep_consider_slot:
    cmpq %rcx, %rbx   # check if its at the edge of the chunk
    je sweep_chunk_done

    slot_size %rbx %r8   # %r8 := current slot size

    is_marked 7(%rbx)           # if slot is marked
    jnz sweep_unmark_and_move_to_next_slot # move to next slot

    set_free 7(%rbx)            # if not, mark it as free

    mov %rbx, %r12
    addq %r8, %r12

    cmpq %rcx, %r12   # check if its at the edge of the chunk
    je sweep_chunk_done

    is_marked 7(%r12)            # check if next slot marked:
    jnz sweep_move_to_next_slot  # if not, it should be freed

    slot_size %r12 %r13
    addq %r13, (%rbx)
    addq %r13, %r8

    jmp sweep_move_to_next_slot

sweep_unmark_and_move_to_next_slot:
    unset_mark 7(%rbx)

sweep_move_to_next_slot:

    addq %r8, %rbx
    jmp sweep_consider_slot

sweep_chunk_done:
    movq (%r10), %r11     # %r11 - next chunk

    cmpq $0, %r11         # if next chunk == NULL
    je sweep_done
    movq %r11, %r10       # else set next as current chunk
    jmp sweep_start

sweep_done:
    ret
