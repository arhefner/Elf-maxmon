; max monitor program
;
; simple monitor for 1802-based systems.
;
; this monitor makes use of the max bios for i/o, as well
; as to set up the rca scrt system for subroutine calls.
;
#include opcodes.def
#include sysconfig.inc
#include bios.inc

#ifndef ROMBASE
#define ROMBASE   08000h
#endif

#ifndef MONSTART
#define MONSTART  0f000h
#endif

#define INIT_CON

            ; Low Memory Usage

findtkn:    equ   0030h                 ; jump vector for f_findtkn
idnum:      equ   0033h                 ; jump vector for f_idnum
devbits:    equ   0036h                 ; f_getdev device present result
clkfreq:    equ   0038h                 ; processor clock frequency in khz
lastram:    equ   003ah                 ; f_freemem last ram address result
type:       equ   003ch                 ; RAM vector for console output
read:       equ   003fh                 ; RAM vector for console input

buffer:     equ   0048h                 ; buffer for I/O
bufend:     equ   007fh
buflen:     equ   bufend-buffer+1

savexp:     equ   0080h
saved:      equ   0081h
savedf:     equ   0082h
regs:       equ   0083h                 ; memory for register save

stack:      equ   00ffh                 ; top of temporary booting stack

bootpg:     equ   0100h                 ; address to load boot block to

m_bpl:      equ   8

            ; Elf/OS Kernel Variables

o_wrmboot:  equ   0303h                 ; kernel warm-boot reinitialization
k_clkfreq:  equ   0470h                 ; processor clock frequency in khz

            org   ROMBASE

entry:      dis                         ; disable interrupts
            db    $00                   ; on reset

            ; Do some basic initialization. Branching to f_initcall will setup
            ; R4 and R5 for CALL, R2 as stack pointer, and finally, R3 as PC
            ; when it returns via RTN.

sysinit:    mov   r2,stack              ; temporary boot stack

            mov   r6,chkdevs            ; return address for f_initcall

            lbr   f_initcall


            ; Discover devices present in the system to store into a memory
            ; variable that getdev will later return when called. Detection
            ; is not necessarily robust but should work on the platform and
            ; configurations it is designed for. It's hard to do better.

chkdevs:    mov   ra,devbits            ; pointer to memory variables

            mov   rb,FREQ_KHZ           ; default processor clock frequency

            ; Setup the device map entry which will later be returned by any
            ; call to f_getdev. Devices that may or may not be present will
            ; be determined at reset time by probing for them.

            ldi   0                     ; device map msb for future use
            str   ra

            ldi   (1<<0)+(1<<2)         ; serial and disk always present
            inc   ra
            str   ra


            ; Discovery of the devices is done by looking for some bits that
            ; should always have particular values. This doesn't guarantee
            ; that some other device isn't at the port, but it should be able
            ; to tell if nothing is there. Since the 1802/Mini bus floats,
            ; reading an unused port normally returns the INP instruction
            ; opcode due to bus capacitance from the fetch machine cycle.

            ; Discovery of UART is done by looking for 110XXXX0 pattern in
            ; status register which should be present once the DA flag is
            ; cleared by reading the data register.

          #if UART_GROUP
            sex   r3
            out   EXP_PORT
            db    UART_GROUP
            sex   r2
          #endif

            inp   UART_STATUS           ; clear status flags
            inp   UART_DATA

            inp   UART_STATUS           ; check for psi and da bits low
            ani   0e1h
            xri   0c0h
            bnz   findrtc

            ldn   ra                    ; looks like uart is present
            ori   (1<<3)
            str   ra


            ; Check for the RTC by looking for XXXX000X at the RTC month
            ; tens digit. Since it can only be 0 or 1 those zero bits will
            ; always be present even if the clock is not setup right.

findrtc:    sex   r3                    ; select rtc month msd register

          #if RTC_GROUP != UART_GROUP
            out   EXP_PORT
            db    RTC_GROUP
          #endif

            out   RTC_PORT
            db    29h

            sex   r2                    ; look for xxxx000x data
            inp   RTC_PORT
            ani   0eh
            bnz   savefrq

            ldn   ra                    ; looks like rtc is present
            ori   (1<<4)
            str   ra


            ; If we have an RTC, we can use it to measure the processor
            ; frequency, so do that now.

            sex   r3                    ; setup rtc for 64 hz pulse mode
            out   RTC_PORT
            db    2fh
            out   RTC_PORT
            db    14h
            out   RTC_PORT
            db    2eh
            out   RTC_PORT
            db    10h
            out   RTC_PORT
            db    2dh
            out   RTC_PORT
            db    10h

            ldi   0                     ; start frequency count at 0
            plo   rb
            phi   rb

            ldi   4                     ; number of pulses to measure
            plo   rc

            sex   r2                    ; needed for inp to be safe


            ; First syncronize to the falling edge of a FLAG pulse.

freqlp1:    inp   RTC_PORT              ; wait for low signal to sync
            ani   4
            bnz   freqlp1

freqlp2:    inp   RTC_PORT              ; wait for high signal to sync
            ani   4
            bz    freqlp2


            ; Now time the next full cycle of FLAG. We increment twice for
            ; two reasons, one is that it gives the proper loop timing (10
            ; machine cycles) for the math to work out with integral numbers,
            ; the other is that we need a multiply to two in here anyway since
            ; the final ratio of counts to frequency is 32/25.

freqlp3:    inc   rb                    ; wait for low signal to count
            inc   rb
            inp   RTC_PORT
            ani   4
            bnz   freqlp3

freqlp4:    inc   rb                    ; wait for high signal to count
            inc   rb
            inp   RTC_PORT
            ani   4
            bz    freqlp4

            inc   rb                    ; loop while still keeping count
            inc   rb
            dec   rc
            glo   rc
            bnz   freqlp3


            ; Multiply the result by 16/25 to get the final answer, but do
            ; it by multiplying by 4/5 twice so we don't overflow 16 bits.
            ; The calculated result will be stored after all devices are
            ; probed in case another device wants to override the result.

            ldi   2
            plo   rc

hzratio:    glo   rb                    ; multiply by 2 while moving to rf
            shl
            plo   rf
            ghi   rb
            shlc
            phi   rf

            glo   rf                    ; multiply by 2 again so 4 total
            shl
            plo   rf
            ghi   rf
            shlc
            phi   rf

            ldi   5.1                   ; divide by 5
            phi   rd
            ldi   5.0
            plo   rd

            call  div16

            dec   rc                    ; loop the 4/5 multiply twice
            glo   rc
            bnz   hzratio


            ; Store the processor clock frequency to it's memory variable.

savefrq:    inc   ra                    ; move on from device map

            ghi   rb                    ; save processor frequency
            str   ra
            inc   ra
            glo   rb
            ani   -2
            str   ra
            inc   ra


            ; Initialize the jump vectors for the BIOS API calls that have
            ; been moved to loadable modules. Install for each one a LBR
            ; instruction to O_WRMBOOT which the module will overwrite the
            ; address when its loaded. This will at least fail gracefully
            ; if they are called when the module is not loaded.

            mov   rf,findtkn            ; get address of first vector

            ldi   2                     ; two of them to populate
            plo   re

tknloop:    ldi   0c0h                  ; write lbr opcode and address
            str   rf
            inc   rf
            ldi   o_wrmboot.1
            str   rf
            inc   rf
            ldi   o_wrmboot.0
            str   rf
            inc   rf

            dec   re                    ; loop for all
            glo   re
            bnz   tknloop


            ; It's not safe to run the expansion memory enable and memory
            ; scan code from ROM for two reasons: we are running from part
            ; of the ROM that will disappear once RAM is switched in, and
            ; attempting to write the EEPROM will cause a write cycle that
            ; makes it temporarily unreadable, even when software protected.
            ;
            ; So copy these routines to RAM in the boot sector page first,
            ; then run it from there, and we will jump to BIOS boot after.

            ldi   raminit.1             ; get start of code to copy
            phi   rc

            ldi   bootpg.1              ; get address to copy to
            phi   rd

            ldi   raminit.0             ; lsb is same for both
            plo   rc
            plo   rd

            ldi   (endinit-raminit).1   ; number of bytes to copy
            phi   re
            ldi   (endinit-raminit).0
            plo   re

cpyloop:    lda   rc                    ; copy each byte to ram
            str   rd
            inc   rd
            dec   re
            glo   re
            bnz   cpyloop
            ghi   re
            bnz   cpyloop

            ldi   bootpg.1              ; jump to copy in ram
            phi   r3


            ; Enable expander card memory (this code runs from low RAM).
            ; If the expander card is not present, this does nothing.

raminit:    sex   r3                    ; enable banked ram

          #ifdef EXP_MEMORY
            out   RTC_PORT
            db    81h
          #endif

          #if RTC_GROUP
            out   EXP_PORT              ; make sure default expander group
            db    NO_GROUP
          #endif

            ; For development purposes, NO_RAMCHECK can be defined.
            ; This skips the RAM check and sets the last RAM address to
            ; immediately before the resident portion of the BIOS. This
            ; allows the BIOS to be loaded in RAM and be preserved when
            ; loading the os.

#ifndef NO_RAMCHECK

            ; Find the last address of RAM present in the system. The search
            ; is done with a granularity of one page; this is not reliable
            ; on systems that do not have an integral number of pages of RAM.
            ;
            ; This implementation is safe for systems with EEPROM, which will
            ; go into a write cycle when a write is attempted to it, even when
            ; software write-protected. When this happens, data read is not
            ; valid until the end of the write cycle. The safety of this
            ; routine in this situation is accomplished by copying the code
            ; into RAM and running it from there instead of from ROM. This
            ; is run once at system initialization and the value stored and
            ; later that stored value is returned by f_freemem.
            ;
            ; In case this is run against an EEPROM that is not software
            ; write-protected (not recommended) this attempts to randomize
            ; the address within the page tested to distribute wear across
            ; the memory cells in the first page of the EEPROM.

            ldi   3fh                   ; we must have at least 16K
            phi   rf

            sex   rf                    ; rf is pointer to search address

scnloop:    glo   rf                    ; randomize address within page
            xor
            plo   rf

            ghi   rf                    ; advance pointer to next page
            adi   1
            phi   rf

            ldi   0                     ; get contents of memory, save it,
            xor                         ;  then complement it
            plo   re
            xri   255

            str   rf                    ; write complement back, then read,
            xor                         ;  if not the same then set df
            adi   255

            glo   re                    ; restore original value
            str   rf
  
scnwait:    glo   re                    ; wait until value just written
            xor                         ;  reads back again
            bnz   scnwait

            bnf   scnloop

#else
            mov   rf,himem
#endif

            ghi   rf
            smi   1
            str   ra

            inc   ra
            ldi   0ffh
            str   ra

            lbr   bootpg+0100h

            .align page

bootmsg:    ldi   devbits.1             ; pointer to memory variables
            phi   ra
            ldi   devbits.0
            plo   ra

          #ifdef INIT_CON
            call  f_setbd
          #endif

            call  f_inmsg
            db    13,10
            db    'MBIOS 3.1.0',13,10
            db    'Devices: ',0

            inc   ra
            lda   ra
            plo   rb

            ghi   r3
            phi   rd
            ldi   devname.0
            plo   rd

devloop:    glo   rb
            shr
            plo   rb
            bnf   skipdev

            ghi   rd
            phi   rf
            glo   rd
            plo   rf

            call  f_msg

            ldi   ' '
            call  f_type

skipdev:    lda   rd
            bnz   skipdev

            ldn   rd
            bnz   devloop

            call  f_inmsg
            db    13,10
            db    'Clock: ',0

            mov   rf,buffer

            lda   ra
            phi   rd
            lda   ra
            plo   rd

            call  f_uintout

            ldi   0
            str   rf

            mov   rf,buffer
            call  f_msg

            call  f_inmsg
            db    ' KHz'13,10
            db    'Memory: ',0

            ldi   0
            phi   rd
            lda   ra
            shr
            shr
            adi   1
            plo   rd

            mov   rf,buffer
            call  f_uintout

            ldi   0
            str   rf

            mov   rf,buffer
            call  f_msg

            call  f_inmsg
            db    ' KB',13,10
            db    13,10,0


            ; Now that all initialization has been done, start the monitor by
            ; simply jumping to intro.

            lbr   intro

            ; Divide two 16-bit numbers to get a 16-bit result plus a 16-bit
            ; remainder. The input numbers are in RF and RD and the result
            ; RF/RD is returned in RB with the remainder in RF.
 
div16:      ghi   re                    ; temporary place for subtraction lsb
            stxd

            ldi   16                    ; number of bits to process
            plo   re

            glo   rf                    ; transfer dividend to quotient while
            shl                         ;  shifting out first dividend bit
            plo   rb
            ghi   rf
            shlc
            phi   rb

            ldi   0                     ; clear remainder to start
            plo   rf
            phi   rf

divloop:    glo   rf                    ; shift dividend bit into remainder
            shlc
            plo   rf
            ghi   rf
            shlc
            phi   rf

            glo   rd                    ; subtract divisor from remainder
            str   r2                    ;  but do not update remainder yet
            glo   rf
            sm
            phi   re
            ghi   rd
            str   r2
            ghi   rf
            smb

            bnf   divskip               ; if negative do not update remainder

            phi   rf                    ; transfer difference to remainder
            ghi   re
            plo   rf

divskip:    glo   rb                    ; shift borrow bit into result and
            shlc                        ;  shift dividend bit into remainder
            plo   rb
            ghi   rb
            shlc
            phi   rb

            dec   re                    ; repeat until all 16 bits done
            glo   re
            bnz   divloop

            irx                         ; restore temporary register
            ldx
            phi   re

            rtn                         ; return to caller

            ;   0: IDE-like disk device
            ;   1: Floppy (no longer relevant)
            ;   2: Bit-banged serial
            ;   3: UART-based serial
            ;   4: Real-time clock
            ;   5: Non-volatile RAM

devname:    db  'IDE',0                 ; bit 0
            db  'Floppy',0              ; bit 1
            db  'Serial',0              ; bit 2
            db  'UART',0                ; bit 3
            db  'RTC',0                 ; bit 4
            db   0

endinit:    equ   $

            org   MONSTART

himem:      equ   $-1

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

r_cmd:      call  skipws
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
            lbnz  bad_parm              ; there should be no parameters
            call  f_inmsg
            db    "Start send...",0
            call  loadbin
            call  f_inmsg
            db    "done",13,10,0
            lbr   prompt

intro:      call  f_inmsg
            db    13,"Max Monitor v4.1",13,10,0

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
            smi   $09
            lbz   l_cmd
            smi   $01
            lbz   m_cmd
            smi   $05
            lbz   r_cmd
            smi   $01
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
            ghi   ra
            stxd
            glo   ra
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
            phi   ra
            call  f_type

            call  f_read
            plo   ra
            call  f_type

readlp:     call  f_read

            str   ra
            inc   ra

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
            plo   ra
            ldxa
            phi   ra
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

            end   entry
