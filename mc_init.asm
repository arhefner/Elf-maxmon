; Max Monitor initialization
;
; Initialization program for the Max Monitor.
;
; This is a version of the monitor initialization routine for
; the 1802 Membership card.
#include max_mon.inc
#include monitor.inc

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

            ldi   (1<<2)                ; serial always present

            inc   ra
            str   ra

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
            ldi   m_break.1
            str   rf
            inc   rf
            ldi   m_break.0
            str   rf
            inc   rf

            dec   re                    ; loop for all
            glo   re
            bnz   tknloop

            mov   rf,HIMEM

            ghi   rf
            smi   1
            str   ra

            inc   ra
            ldi   0ffh
            str   ra

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
            ; simply jumping to MONSTART.

            lbr   MONSTART

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

            end  entry