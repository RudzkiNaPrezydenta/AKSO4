global ninv
section .text

; void ninv(uint64_t *y, uint64_t *x, uint64_t n)
; y: rdi, x: rsi, n: rdx

ninv:
    ; prologue
    push rbp
    mov  rbp, rsp

    ; Save registers
    push r12
    push r13
    push r14
    push r15
    push rbx

    ; ----------------------------------------
    ; allocate 3*(n/8) bytes on stack
    ; ----------------------------------------
    mov  rax, rdx
    shr  rax, 3           ; rax = n/8 (bytes per array)
    mov  rcx, rax
    imul rax, 3
    sub  rsp, rax

    mov  rbx, rsp         ; first array
    mov  r12, rbx
    add  r12, rcx         ; second
    mov  r13, r12
    add  r13, rcx         ; third

    ; ----------------------------------------
    ; find first significant bit of x
    ; ----------------------------------------
    mov  r8, rdx
    shr  r8, 6            ; m = n/64
    mov  r15, r8
    xor  r9, r9

.find_loop:
    dec  r15
    mov  rax, [rsi + r15*8]
    test rax, rax
    jnz  .found
    cmp  r15, 0
    jne  .find_loop
    jmp  .done

; ==================================================
;              power-of-two detection
; ==================================================
.found:
    bsr  r10, rax                 ; r10 = bit index in the word (0..63)
    mov  r9, r15                  ; r9  = word index

    ; Check if this word is a power of two
    mov  r11, rax
    dec  r11
    test rax, r11
    jnz  .x_is_not_a_power_of_2   ; more than one bit in this word

    ; Ensure all lower words are zero
    xor  r11, r11
.check_lower_words:
    cmp  r11, r9
    jae  .x_is_a_power_of_2
    mov  rdx, [rsi + r11*8]
    test rdx, rdx
    jnz  .x_is_not_a_power_of_2
    inc  r11
    jmp  .check_lower_words

.x_is_a_power_of_2:
    mov r8, rdx
    mov r11, r9
    shl r11, 6                    ; r11 = w*64
    sub r8, r11                   ; r8 = m*64 - w*64
    mov r11, r10                  ; r11 = b
    sub r8, r11                   ; r8 = m*64 - w*64 - b
    mov r11, r8
    shr r8, 6                     ; r8 = word index in y
    shl r8, 6
    sub r11, r8                   ; r11 = bit index in y
    shr r8, 6

    ; zero y first
    mov  r15, rdx
    shr  r15, 6
    xor  r14, r14
.zero_y_power2:
    cmp  r14, r15
    jae  .after_zero_y_power2
    mov  qword [rdi + r14*8], 0
    inc  r14
    jmp  .zero_y_power2

.after_zero_y_power2:
    mov rax, 1
    mov rcx, r11
    shl rax, cl
    mov [rdi + 8*r8], rax         ; y = single bit
    jmp .finish

.x_is_not_a_power_of_2:
    mov  r8, rdx                  ; n
    mov  r11, r9
    shl  r11, 6
    add  r11, r10                 ; a = w*64 + b
    sub  r8, r11
    dec  r8                       ; n - a - 1

    ; compute (word, bit) directly
    mov  r11, r8
    mov  rax, r11
    shr  rax, 6                   ; word = (n-a-1)/64
    mov  r8, rax
    and  r11, 63                  ; bit  = (n-a-1) % 64

    ; zero y first
    mov  r15, rdx
    shr  r15, 6
    xor  r14, r14
.zero_y:
    cmp  r14, r15
    jae  .after_zero_y
    mov  qword [rdi + r14*8], 0
    inc  r14
    jmp  .zero_y
.after_zero_y:

    mov  rax, 1
    mov  rcx, r11
    shl  rax, cl
    mov  [rdi + 8*r8], rax        ; seed y with first bit
    jmp  .build_y                 ; continue building

; ===============================================
;      PREP FOR BUILDING THE REST OF Y
; ===============================================
.build_y:
    mov  r15, rdx
    shr  r15, 6                ; m

    ; -------- zero tmp (rbx) --------
    xor  r14, r14
.zero_rbx:
    cmp  r14, r15
    jae  .after_zero_rbx
    mov  qword [rbx + r14*8], 0
    inc  r14
    jmp  .zero_rbx
.after_zero_rbx:

    ; r15 = index of first significant x word
    mov  r15, r9
    ; r14 = m-1 (top index in tmp)
    mov  r14, rdx
    shr  r14, 6
    dec  r14

.move_x_to_rbx_wordwise:
    mov  rax, qword [rsi + 8*r15]
    mov  qword [rbx + 8*r14], rax
    dec  r14
    dec  r15
    js   .move_x_to_rbx_bitwise
    jmp  .move_x_to_rbx_wordwise

; -------- shift tmp left by r11 bits (0..63) --------
.move_x_to_rbx_bitwise:
    mov  rcx, r11
    test rcx, rcx
    jz   .after_bit_shift

    mov  r14, rdx
    shr  r14, 6                ; m
    test r14, r14
    jz   .after_bit_shift

    xor  r15, r15              ; i = 0
    mov  rax, [rbx + r15*8]
    shl  rax, cl
    mov  [rbx + r15*8], rax
    inc  r15

.shift_left_loop:
    cmp  r15, r14
    jae  .after_bit_shift
    mov  rdx, [rbx + r15*8]        ; dest
    mov  rax, [rbx + (r15-1)*8]    ; prev
    shl  rdx, cl                   ; dest <<= k

    mov  r8, 64
    sub  r8, rcx                   ; r8 = 64 - k
    mov  r10, rax
    mov  ecx, r8d                  ; load into ecx so cl = (64-k)
    shr  r10, cl                   ; prev >> (64-k)

    or   rdx, r10
    mov  [rbx + r15*8], rdx

    mov  rcx, r11                  ; restore k
    inc  r15
    jmp  .shift_left_loop

.after_bit_shift:

; -------- set r12 := rbx and r13 := rbx --------
.continue_building_y:
    mov  r15, rdx
    shr  r15, 6                    ; m
    xor  r14, r14
.copy_rbx_to_r12_r13:
    cmp  r14, r15
    jae  .build_loop
    mov  rax, qword [rbx + r14*8]
    mov  qword [r12 + r14*8], rax
    mov  qword [r13 + r14*8], rax
    inc  r14
    jmp  .copy_rbx_to_r12_r13

; ===============================================
;      MAIN LOOP TO BUILD Y
; ===============================================
.build_loop:
    mov  r15, rdx
    shr  r15, 6                    ; m
    xor  r14, r14

.change_bit:
    dec  r11
    jns  .build_loop
    mov  r11, 0x3F
    dec  r8
    js   .finish
    clc

; -------- shift r12 right by 1 (multiword) --------
.shift_r12_right_by_1:
    mov  r14, r15                 ; r14 = m
    clc
.shift_r12_rcr_loop:
    dec  r14
    mov  rax, [r12 + r14*8]
    rcr  rax, 1
    mov  [r12 + r14*8], rax
    test r14, r14                 ; branch on loop counter, not rcr result
    jnz  .shift_r12_rcr_loop

; -------- add r12 into rbx with carry --------
.add_r12_to_rbx:
    clc
    xor  r14, r14
.add_loop:
    cmp  r14, r15
    jae  .check_if_carry
    mov  rax, [r12 + r14*8]
    adc  qword [rbx + r14*8], rax
    inc  r14
    jmp  .add_loop

.check_if_carry:
    jnc  .no_carry_happened

    ; carry happened → restore rbx = r13
    mov  r14, r15
.carry_copy_back:
    dec  r14
    mov  rax, [r13 + r14*8]
    mov  [rbx + r14*8], rax
    jnz  .carry_copy_back
    jmp  .build_loop

.no_carry_happened:
    mov  rax, 1
    mov  rcx, r11
    shl  rax, cl
    add  [rdi + 8*r8], rax

    mov  r14, r15
.copy_rbx_to_r13:
    dec  r14
    mov  rax, [rbx + r14*8]
    mov  [r13 + r14*8], rax
    jnz  .copy_rbx_to_r13
    jmp  .build_loop

; ===============================================
;                FINISH / EPILOGUE
; ===============================================
.finish:
    ; free stack
    mov  rax, rdx
    shr  rax, 3
    imul rax, 3
    add  rsp, rax

    ; restore regs
    pop  rbx
    pop  r15
    pop  r14
    pop  r13
    pop  r12

    mov  rsp, rbp
    pop  rbp
    ret

.done:
    jmp .finish