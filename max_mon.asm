; Max Monitor program
;
; Simple monitor for 1802-based systems.
;
; This monitor makes use of the Elf/OS mbios for i/o.
;
#include max_mon.inc

m_bpl:      equ   8

            org   MONSTART

himem:      equ   $

intro:      call  f_inmsg
            db    13,"Max Monitor v4.2",13,10,0

prompt:     b4    $
            call  f_inmsg
            db    "> ",0
            call  readln
            dw    buffer
            db    buflen
            lbnf  process
            call  f_inmsg
            db    "^c",13,10,0
            lbr   prompt

            ; Whenever the monitor starts a program running, it will
            ; initialize R(1) with the address of the break routine.
            ; Provided that the program does not use R(1) or R(0) for
            ; anything, a breakpoint can be placed in the program with
            ; a two-byte sequence:
            ;
            ;     mark      ; $79
            ;     sep   r1  ; $D1
            ;
            ; R(1) is normally the interrupt vector and R(0) is the
            ; DMA pointer. This code will save all of the user's
            ; registers, except R(0) and R(1), but it does manage to
            ; save X, P, D, and DF.
            ;
            ; The 'c' (continue) command branches here as the final step
            ; in restoring the user's context. This restores the original
            ; X and P registers, while leaving R(1) pointed at break:
            ; once again.
breakx:     dis                         ; restore X,P with interrupts off

            ; Save the current D and DF by assuming there's a valid stack
            ; pointer in R(2). The mark instruction executed by the caller
            ; has pushed (X,P)
break:      sex   r2
            stxd                        ; save D to the stack
            shlc                        ; put DF in the lsb
            str   r2                    ; and save that too
            
            ; save registers R(F) through R(2) in memory at regs
            
            mov   r0,regs+32-1
            sex   r0
            push  rf
            push  re
            push  rd
            push  rc
            push  rb
            push  ra
            push  r9
            push  r8
            push  r7
            push  r6
            push  r5
            push  r4
            push  r3
            push  r2

            ; R(1) and R(0) don't contain useful data; skip over them
            ; so we can save D, DF, and xor

            ldi   0
            stxd
            stxd
            stxd
            stxd

            ; recover DF, D, and X from user stack and save in our memory

            lda   r2                    ; get df
            stxd                        ; store at savedf:
            lda   r2                    ; get D
            stxd                        ; store at saved:
            ldn   r2                    ; get X
            stxd                        ; store at savexp:

            ; update the saved R2 so it shows the correct value, before we
            ; pushed 3 bytes on the stack

            mov   r0,regs+5
            push  r2

            ; everything is saved, now do a minimal initialization so the
            ; monitor will work

            mov   r2,stack
            mov   r6,break2
            lbr   f_initcall

break2:
#ifdef INIT_CON
            call  f_setbd
#endif

            lbr   dispregs

savechr:    glo   rb
            smi   $20
            bl    notprt
            glo   rb
            smi   $7e
            bge   notprt
            glo   rb
            lskp
notprt:     ldi   $2e
            str   rf
            inc   rf
            rtn

m_cmd:      call  skipws
            ldn   rf                    ; if no params,
            bz    m_cont                ; repeat last command
            call  f_hexin               ; get the start address
            ghi   rd                    ; store in r7
            phi   r7
            glo   rd
            plo   r7
            call  skipws
            call  f_atoi                ; get the length
            lbdf  bad_parm
            ghi   rd                    ; store in r8
            phi   r8
            glo   rd
            plo   r8
            call  skipws
            ldn   rf                    ; check for extra
            lbnz  bad_parm              ; parameters
m_cont:     mov   r9,r8                 ; copy the byte count to r9
m_loop:     ghi   r9                    ; check if there are
            bnz   m_full                ; more than a single
            glo   r9                    ; line of bytes remaining
            lbz   m_done                ; done if no bytes remaining
            smi   m_bpl
            bge   m_full
            xri   $ff                   ; less than a full line
            adi   $01
            phi   ra                    ; keep the difference in ra.1
            glo   r9                    ; set number of bytes to
            plo   ra                    ; remaining, or to
            br    m_line
m_full:     ldi   m_bpl                 ; the number of bytes per line
            plo   ra
            ldi   $00
            phi   ra
m_line:     mov   rf, buffer            ; set buffer pointer for ascii
            ldi   $20                   ; store leading space
            str   rf
            inc   rf
            ghi   r7                    ; print the address of the line
            phi   rd
            glo   r7
            plo   rd
            call  hex4out
m_byte:     call  f_inmsg               ; space before next byte
            db    $20, $00
            lda   r7                    ; print next byte
            plo   rb
            call  hex2out
            call  savechr               ; save ascii in buffer
            dec   r9                    ; decrement total count
            dec   ra                    ; loop for the number of
            glo   ra                    ; bytes on the line
            bnz   m_byte
            ghi   ra                    ; pad bytes necessary?
            bz    m_eol
            plo   ra
m_fill:     call  f_inmsg
            db    $20, $20, $20, $00
            dec   ra
            glo   ra
            bnz   m_fill
m_eol:      ldi   $00
            str   rf
            mov   rf, buffer
            call  f_msg
            call  f_inmsg
            db    $0d, $0a, $00
            b4    m_done
            lbr   m_loop
m_done:     lbr   prompt

g_cmd:      call  skipws
            call  f_hexin
            call  skipws
            ldn   rf
            lbnz  bad_parm
            ; load run address into r0
            mov   r0,rd
            ; load monitor breakpoint address into r1
            mov   r1,break
            sex   r0
            ; jump to new program
            sep   r0

c_cmd:      call  skipws
            ldn   rf
            lbnz  bad_parm              ; there should be no parameters
            ; use R1 as the PC
            mov   r1,restore
            sep   r1
restore:    ; restore registers R(2) thru R(F)
            mov   r0,regs+3
            sex   r0
            pop   r2
            pop   r3
            pop   r4
            pop   r5
            pop   r6
            pop   r7
            pop   r8
            pop   r9
            pop   ra
            pop   rb
            pop   rc
            pop   rd
            pop   re
            pop   rf

            ; Now recover (X,P), D, and DF. Unfortunately, the only way to do
            ; this is to temporarily push them on the user's stack first,
            ; which works so long as R(2) points to a valid stack.

            mov   r0,savexp
            ldxa                        ; get (X,P)

            ; Note that the final instruction we execute, DIS, does
            ; M(R(X))->(X,P) first, and R(X)+1 second. So we have to decrement
            ; the user's stack pointer first, which has the effect of wasting
            ; a byte, so that in the end it comes out right.

            dec   r2                    ; save them on user's stack
            str   r2
            ldxa                        ; load D from saved
            dec   r2                    ; save that on user's stack
            str   r2
            ldxa                        ; finally load savedf
            shrc                        ; restore DF
            sex   r2                    ; switch to user's stack
            ldxa                        ; and restore d

            ; The last instruction is a minor trick. Restore (X,P) from the
            ; user's stack while at the same time leaving R(1) (our current
            ; PC) pointing to break:. The contents of R(0) are never restored.

            lbr   breakx

l_cmd:      call  skipws
            ldn   rf
            bnz   laddr
            mov   ra,0
            br    load
laddr:      call  f_hexin
            call  skipws
            ldn   rf
            lbnz  bad_parm
            ; load run address into r0
            mov   ra,rd
load:       call  f_inmsg
            db    "Start send...",0
            call  loadbin
            call  f_inmsg
            db    "done",13,10,0
            lbr   prompt

v_cmd:      call  skipws
            ldn   rf
            lbnz  bad_parm              ; there should be no parameters
dispregs:   mov   r1,savexp
dispxp:     call  f_inmsg
            db    13,10,"P=",0
            lda   r1
            phi   rb
            plo   rb
            call  hexdig
            glo   rb
            call  f_type
            call  f_inmsg
            db    " X=",0
            ghi   rb
            shr
            shr
            shr
            shr
            plo   rb
            call  hexdig
            glo   rb
            call  f_type

dispd:      call  f_inmsg
            db    13,10,"D=",0
            lda   r1
            plo   rb
            call  hex2out
            call  f_inmsg
            db    13,10,0

dispdf:     call  f_inmsg
            db    "DF=",0
            lda   r1
            shrc
            bnf   df0
            call  f_inmsg
            db    "1",13,10,0
            br    reg0
df0:        call  f_inmsg
            db    "0",13,10,0

reg0:       call  f_inmsg
            db    "R0=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " R1=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    13,10,"R2=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " R3=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    13,10,"R4=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " R5=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    13,10,"R6=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " R7=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    13,10,"R8=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " R9=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    13,10,"RA=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " RB=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    13,10,"RC=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " RD=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    13,10,"RE=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out
            call  f_inmsg
            db    " RF=",0
            lda   r1
            plo   rd
            lda   r1
            phi   rd
            call  hex4out

            call  f_inmsg
            db    13,10,0

            lbr   prompt

process:    call  f_inmsg
            db    13,10,0

            mov   rf,buffer

            call  skipws                ; skip leading spaces
            lda   rf
            lbz   prompt                ; reached end-of-line
            ani   $df                   ; decode command
            smi   $42
            lbz   b_cmd
            smi   $01
            lbz   c_cmd
            smi   $04
            lbz   g_cmd
            smi   $05
            lbz   l_cmd
            smi   $01
            lbz   m_cmd
            smi   $06
            lbz   s_cmd
            smi   $03
            lbz   v_cmd
            smi   $01
            lbz   w_cmd
            smi   $03
            lbz   z_cmd
            call  f_inmsg
            db    "Bad command.",13,10,0
            lbr   prompt

.align      page

s_cmd:      call  skipws
            call  f_hexin
            mov   ra,rd
            call  skipws
            call  f_hexin
            call  skipws
            ldn   rf
            lbnz  bad_parm
            mov   rc,rd
            sub16 rc,ra
            bge   s_start
            call  f_inmsg
            db    "end < start",13,10,0
            br    s_ret
s_start:    inc   rc
            call  f_inmsg
            db    "Start receive...",0
            call  savebin
            bnf   s_done
            call  f_inmsg
            db    "error",13,10,0
s_done:     call  f_inmsg
            db    "done",13,10,0
s_ret:      lbr   prompt

w_cmd:      call  skipws
            call  f_hexin               ; get the address for writing
            ghi   rd                    ; store it in r7
            phi   r7
            glo   rd
            plo   r7
w_next:     call  skipws                ; look for next data
            ldn   rf
            lbz   prompt                ; at eol, we're done
            call  f_hexin               ; check for hex byte
            ldn   rf
            bz    w_store               ; if it's eol
            smi   $20                   ; or if it's a space
            bz    w_store               ; store the byte
            smi   $02                   ; start of string?
            lbnz  bad_parm              ; neither, bad parameter
            inc   rf
            br    w_string
w_store:    ghi   rd
            lbnz  bad_parm
            glo   rd
            str   r7
            inc   r7
            br    w_next
w_string:   ldn   rf                    ; get next char of string
            bz    w_end                 ; end of line?
            xri   $22                   ; ending quote
            bnz   w_str                 ; no, store character
            inc   rf                    ; yes, skip the '"'
            br    w_next                ; and look for more input
w_str:      ldn   rf
            str   r7
            inc   r7
            inc   rf
            br    w_string
w_end:      call  skipws
            ldn   rf
            lbnz  bad_parm
            lbr   prompt

bad_parm:   call  f_inmsg
            db    "Bad parameter",0
            ldn   rf
            bz    no_disp
            call  f_inmsg
            db    ": '",0
            call  f_msg
            call  f_inmsg
            db    "'",0
no_disp:    call  f_inmsg
            db    13,10,0
            lbr   prompt

            ; this updates the kernel variable containing the processor clock
            ; frequency since there is no other way for that to happen

b_cmd:      mov   rc,clkfreq            ; get address of bios variable
            mov   rd,k_clkfreq          ; get address of kernel variable

            lda   rc                    ; update kernel with clock freq
            str   rd
            inc   rd
            lda   rc
            str   rd

            mov   r0,f_boot
            sex   r0
            sep   r0

z_cmd:      call  skipws
            call  f_hexin               ; get the start address
            ghi   rd                    ; store in r7
            phi   r7
            glo   rd
            plo   r7
            call  skipws
            call  f_atoi                ; get the length
            lbdf  bad_parm
            ghi   rd                    ; store in r8
            phi   r8
            glo   rd
            plo   r8
            call  skipws
            ldn   rf                    ; check for extra
            lbnz  bad_parm              ; parameters
            ldi   $00
            plo   rd
loop:       glo   rd
            str   r7
            inc   r7
            dec   r8
            lbrnz r8,loop
            lbr   prompt

;------------------------------------------------------------------------
;routine to skip whitespace in the input buffer.
skipws:     lda   rf
            xri   $20
            bz    skipws
            dec   rf
            rtn

.align      para

hextab:     db    '0'
            db    '1'
            db    '2'
            db    '3'
            db    '4'
            db    '5'
            db    '6'
            db    '7'
            db    '8'
            db    '9'
            db    'a'
            db    'b'
            db    'c'
            db    'd'
            db    'e'
            db    'f'

;------------------------------------------------------------------------
;routine to convert the low nybble of rb to an ascii hex digit.
hexdig:     ghi   rc                    ; save registers
            stxd
            glo   rc
            stxd

            mov   rc,hextab             ; point to the start of the hex table
            glo   rb
            ani   $0f                   ; isolate the low nybble
            str   r2
            glo   rc                    ; add to hex table pointer
            add
            plo   rc
            ldn   rc                    ; get ascii value from table
            plo   rb

            irx                         ; restore registers
            ldxa
            plo   rc
            ldx
            phi   rc

            rtn

;------------------------------------------------------------------------
;routine to output a data byte as two hex digits.
;
;rb.0 = 8-bit value to be written.
hex2out:    ghi   rb                    ; save registers
            stxd
            glo   rb
            stxd
            phi   rb                    ; save a copy of input byte in rb.1
            shr                         ; get the high-order nybble
            shr
            shr
            shr
            plo   rb                    ; output the high digit
            call  hexdig
            glo   rb
            call  f_type
            ghi   rb                    ; get the original byte back
            ani   $0f                   ; isolate the low nybble
            plo   rb                    ; output the low digit
            call  hexdig
            glo   rb
            call  f_type
            irx                         ; restore registers
            ldxa
            plo   rb
            ldx
            phi   rb
            rtn

;------------------------------------------------------------------------
;routine to output a data word as four hex digits.
;
;rd = 16-bit value to be written
hex4out:    glo   rb
            stxd
            ghi   rd
            plo   rb
            call  hex2out
            glo   rd
            plo   rb
            call  hex2out
            irx
            ldx
            plo   rb
            rtn

;------------------------------------------------------------------------
;routine to read a string from the serial port.
;pointer to buffer and buffer size passed as parameters.
readln:     push  rf                    ; save registers
            push  rc
            lda   r6                    ; get pointer to buffer
            phi   rf
            lda   r6
            plo   rf
            ldi   $00
            phi   rc
            lda   r6                    ; get size of buffer
            plo   rc                    ; save it in rc
            call  f_inputl
            pop   rc
            pop   rf
            rtn

            .align page

;------------------------------------------------------------------------
;routine to load a binary file from the serial port.
loadbin:    ghi   r8
            stxd
            glo   r8
            stxd
            ghi   r9
            stxd
            glo   r9
            stxd
            ghi   rc
            stxd
            glo   rc
            stxd

            ghi   re
            phi   r8
            ani   0feh
            phi   re

            call  f_read
            xri   55h
            bnz   lberror
            ldi   0aah
            call  f_type

lbnext:     call  f_read
            bz    lbover
            call  f_type
            
            smi   1
            bnz   lberror

            call  f_read
            phi   rc
            call  f_type

            call  f_read
            plo   rc
            dec   rc
            call  f_type

            call  f_read
            phi   r9
            call  f_type

            call  f_read
            plo   r9
            plo   r8
            add16 r9,ra
            glo   r8
            call  f_type

readlp:     call  f_read

            str   r9
            inc   r9

            untl  rc,readlp

ack:        ldi   0aah
            call  f_type

            br    lbnext

lbover:     call  f_read
            xri   'x'
            bnz   lberror

lbdone:     clc
            lskp
lberror:    stc

            ghi   r8
            phi   re

            irx
            ldxa
            plo   rc
            ldxa
            phi   rc
            ldxa
            plo   r9
            ldxa
            phi   r9
            ldxa
            plo   r8
            ldx
            phi   r8

            rtn

            .align page

;------------------------------------------------------------------------
;routine to save a binary file in max format.
;
;on entry:
;  ra = start address of saved data
;  rc = count of bytes to save
;
savebin:    ghi   r8
            stxd
            ghi   rb
            stxd
            glo   rb
            stxd

            ghi   re
            phi   r8
            ani   0feh
            phi   re

            call  f_read
            xri   0aah
            bnz   sberror

            ldi   55h
            call  f_type

sbnext:     mov   rb,rc
            sub16 rb,512
            bpz   send512

            mov   rb,rc
            br    sendblk

send512:    mov   rb,512

sendblk:    sub16 rc,rb

            ldi   01h                   ; send 'receive block' command
            call  f_type

            call  f_read                ; check echo
            xri   01h
            bnz   sberror

            ghi   rb                    ; send high byte of size
            call  f_type

            call  f_read                ; check echo
            str   r2
            ghi   rb
            xor
            bnz   sberror

            glo   rb                    ; send low byte of size
            call  f_type

            call  f_read                ; check echo
            str   r2
            glo   rb
            xor
            bnz   sberror

            ghi   ra                    ; send high byte of address
            call  f_type

            call  f_read                ; check echo
            str   r2
            ghi   ra
            xor
            bnz   sberror

            glo   ra                    ; send low byte of address
            call  f_type

            call  f_read                ; check echo
            str   r2
            glo   ra
            xor
            bnz   sberror

            dec   rb
sendloop:   lda   ra                    ; send block
            call  f_type
            untl  rb,sendloop

            call  f_read                ; check for ack
            xri   0aah
            bnz   sberror

            brnz  rc,sbnext

            ldi   00h                   ; send end command
            call  f_type

            call  f_read
            xri   'x'
            bnz   sberror

sbdone:     clc
            lskp
sberror:    stc

            ghi   r8
            phi   re

            irx
            ldxa
            plo   rb
            ldxa
            phi   rb
            ldx
            phi   r8

            rtn

            .align page

            org   $-9

m_break:    lbr   break
m_loadbin:  lbr   loadbin
m_savebin:  lbr   savebin

            end   intro
