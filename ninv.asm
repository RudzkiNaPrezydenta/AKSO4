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
    shr  r8, 6                      ; r8 = n/64
    mov  r15, r8                    ; r15 = n/64
    xor  r9, r9                     ; r9 = 0

.find_loop:
    dec  r15                        ; r15--
    mov  rax, [rsi + r15*8]         ; load qword from x array
    test rax, rax                   ; check if the loaded value is 0
    jnz  .found                     ; if it's not, jump to .found
    cmp  r15, 0                     ; check if r15 = 0
    jne  .find_loop                 ; if it's not 0, repeat

; ==================================================
;              power-of-two detection
; ==================================================
.found:
    bsr  r10, rax                   ; r10 = bit index in the word (0..63)
    mov  r9, r15                    ; r9  = word index

    ; Check if this word is a power of two
    mov  r11, rax                   ; r11 = rax
    dec  r11                        ; r11 = rax - 1
    test rax, r11                   
    jnz  .x_is_not_a_power_of_2     ; more than one bit in this word

    ; Ensure all lower words are zero
    xor  r11, r11                   ; r11 = 0 (counter)
.check_lower_words:
    cmp  r11, r9                    ; compare word index and r11
    jae  .x_is_a_power_of_2         ; if there are no more words to check, x is a power of 2   
    mov  rax, [rsi + r11*8]         ; rax = word from x
    test rax, rax                   ; check if rax = 0
    jnz  .x_is_not_a_power_of_2     ; if no, x is not a power of 2
    inc  r11                        
    jmp  .check_lower_words         ; if rax = 0, repeat 

; ==================================================
;               x is a power of 2
; ==================================================
.x_is_a_power_of_2:
    mov r8, rdx                     ; r8 = n
    mov r11, r9                     ; r11 = word index of x = w
    shl r11, 6                      ; r11 = (word index of x)*64
    sub r8, r11                     ; r8 = n - w*64
    mov r11, r10                    ; r11 = bit index = b
    sub r8, r11                     ; r8 = n - w*64 - b
    mov r11, r8                     ; r11 = r8
    shr r8, 6                       ; r8 = word index in y
    shl r8, 6                       
    sub r11, r8                     ; r11 = bit index in y
    shr r8, 6
    ; zero y first
    mov r15, rdx                    ; r15 = n
    shr r15, 6                      ; r15 = n/64
    xor r14, r14                    ; r14 = 0 (counter)
.zero_y_power2:
    cmp  r14, r15                   ; compare r14 and r15
    jae  .after_zero_y_power2       ; if r14 >= r15, end the loop
    mov  qword [rdi + r14*8], 0     ; zero a word in y
    inc  r14                        ; r14++
    jmp  .zero_y_power2             ; repeat
.after_zero_y_power2:
    mov rax, 1                      ; rax = 1
    mov rcx, r11                    ; rcx = bit index 
    shl rax, cl                     ; shift 1 left by the bit index 
    mov [rdi + 8*r8], rax           ; y = single bit
    jmp .finish                     ; there's no need to do anything else, so we finish

.x_is_not_a_power_of_2:
    mov  r8, rdx                    ; r8 = n
    mov  r11, r9                    ; r11 = x word index = w
    shl  r11, 6                     ; r11 = w*64
    add  r11, r10                   ; a = w*64 + b
    sub  r8, r11                    ; r8 = n - a
    dec  r8                         ; n - a - 1
    ; compute (word, bit) directly
    mov  r11, r8                    ; r11 = n - a - 1
    mov  rax, r11                   ; rax = n - a - 1
    shr  rax, 6                     ; word = (n-a-1)/64
    mov  r8, rax                    ; r8 = rax
    and  r11, 63                    ; bit  = (n-a-1) % 64
    ; zero y first
    mov  r15, rdx                   ; r15 = n
    shr  r15, 6                     ; r15 = n/64
    xor  r14, r14                   ; r14 = 0
.zero_y:                            
    cmp  r14, r15                   ; compare r14 and r15
    jae  .after_zero_y              ; if r14 >= r15, end the loop
    mov  qword [rdi + r14*8], 0     ; set a word in y to 0
    inc  r14                        ; r14++
    jmp  .zero_y                    ; continue the loop
.after_zero_y:
    mov  rax, 1                     ; rax = 1
    mov  rcx, r11                   
    shl  rax, cl                    ; shift rax
    mov  [rdi + 8*r8], rax          ; seed y with first bit
    jmp  .build_y                   ; continue building

; ===============================================
;      PREP FOR BUILDING THE REST OF Y
; ===============================================
.build_y:
    mov  r15, rdx                   
    shr  r15, 6                     ; r15 = n/64
    xor  r14, r14                   ; r14 = 0
.zero_rbx: 
    cmp  r14, r15                   ; compare r14 and r15
    jae  .after_zero_rbx            ; if r14 >= r15, end the loop
    mov  qword [rbx + r14*8], 0     ; zero a word in rbx
    inc  r14                        ; r14++
    jmp  .zero_rbx                  ; repeat the loop
.after_zero_rbx:
    mov  r15, r9                    ; r15 = index of first significant x word
    mov  r14, rdx
    shr  r14, 6                 
    dec  r14                        ; r14 = m-1 (top index in tmp)

.move_x_to_rbx_wordwise:
    mov  rax, qword [rsi + 8*r15]   ; rax = word in rsi
    mov  qword [rbx + 8*r14], rax   ; word in rbx = word in rsi
    dec  r14
    dec  r15
    js   .move_x_to_rbx_bitwise     ; if r15 < 0 move on to shifting rbx bitwise
    jmp  .move_x_to_rbx_wordwise    ; repeat the loop
.move_x_to_rbx_bitwise:
    mov  rcx, r11                   ; rcx = number of bits to shift
    test rcx, rcx                   
    jz .after_bit_shift             ; if rcx = 0, there's no need to shift anything
    shr rdx, 6                      ; rdx = n/64 (we'll shift it left later)

    mov  rax, [rbx]                 ; rax = least significant word from rbx
    shl  rax, cl                    ; shift word left 
    mov  [rbx], rax                 ; set a word from rbx to rax
    mov  r15, 1                     ; r15 = 1
.shift_left_loop:
    cmp  r15, rdx                   ; compare r15 and rdx
    jae  .after_bit_shift           ; if r15 >= rdx finish shifting
    mov  r14, [rbx + r15*8]         ; destination
    mov  rax, [rbx + (r15-1)*8]     ; previous 
    mov  rcx, r11
    shl  r14, cl                    ; dest <<= k
    mov  r9, 64                     ; r9 = 64
    sub  r9, r11                    ; r9 = 64 - k
    mov  ecx, r9d                   ; load into ecx so cl = (64-k)
    shr  rax, cl                    ; prev >> (64-k)

    or   r14, rax
    mov  [rbx + r15*8], r14

    inc  r15
    jmp  .shift_left_loop

.after_bit_shift:

; -------- set r12 := rbx and r13 := rbx --------
.continue_building_y:
    mov  r15, rdx                   ; r15 = n/64
    shl rdx, 6                      ; rdx = n
    xor  r14, r14                   ; r14 = 0
.copy_rbx_to_r12_r13:
    cmp  r14, r15                   ; compare r14 and r15
    jae  .build_loop                ; if r14 >= r15, finish loop
    mov  rax, qword [rbx + r14*8]   ; rax = [rbx + r14*8]
    mov  qword [r12 + r14*8], rax   ; set word in r12 to rax
    mov  qword [r13 + r14*8], rax   ; set word in r13 to rax
    inc  r14                        
    jmp  .copy_rbx_to_r12_r13

; ===============================================
;              MAIN LOOP TO BUILD Y
; ===============================================
.build_loop:
    mov  r15, rdx                   ; r15 = n
    shr  r15, 6                     ; r15 = n/64
    xor  r14, r14                   ; r14 = 0

.change_bit:
    dec  r11
    jns  .shift_r12_right_by_1
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
    xor r14, r14                   ; r14 = 0
    dec r15
    clc

.add_loop:                          
    cmp  r14, r15                        
    jae  .last_iteration        
    mov  rax, [r12 + r14*8]
    adc  qword [rbx + r14*8], rax
    inc  r14
    jmp  .add_loop

.last_iteration:
    inc r15
    mov  rax, [r12 + r14*8]
    adc  qword [rbx + r14*8], rax

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