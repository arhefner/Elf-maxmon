; Max Monitor initialization
;
; Initialization program for the Max Monitor.
;
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
            ; simply jumping to MONSTART.

            lbr   MONSTART

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

endinit:    equ  $

            end  entry