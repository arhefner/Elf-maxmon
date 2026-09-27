#include opcodes.def
#include sysconfig.inc
#include bios.inc

#ifndef ROMBASE
#define ROMBASE   08000h
#endif

#ifndef RAMBASE
#define RAMBASE   0000h
#endif

#ifndef MONSTART
#define MONSTART  0f000h
#endif

#ifndef HIMEM
#define HIMEM     ROMBASE
#endif

#define INIT_CON

            ; Low Memory Usage

            ; These must match the page zero layout used by the BIOS.

findtkn:    equ   RAMBASE+0030h         ; jump vector for f_findtkn
idnum:      equ   RAMBASE+0033h         ; jump vector for f_idnum
type:       equ   RAMBASE+0036h         ; RAM vector for console output
read:       equ   RAMBASE+0039h         ; RAM vector for console input
devbits:    equ   RAMBASE+003ch         ; f_getdev device present result
clkfreq:    equ   RAMBASE+003eh         ; processor clock frequency in khz
lastram:    equ   RAMBASE+0040h         ; f_freemem last ram address result
romcurr:    equ   RAMBASE+0042h         ; rom disk current block
diskmap:    equ   RAMBASE+0043h         ; disk map, 8 entries
ocrreg:     equ   RAMBASE+004bh         ; sd card ocr register byte

buffer:     equ   RAMBASE+0001h         ; buffer for I/O
bufend:     equ   RAMBASE+002fh
buflen:     equ   bufend-buffer+1

savexp:     equ   RAMBASE+0060h
saved:      equ   RAMBASE+0061h
savedf:     equ   RAMBASE+0062h
regs:       equ   RAMBASE+0063h         ; memory for register save

stack:      equ   RAMBASE+00ffh         ; top of temporary booting stack

bootpg:     equ   RAMBASE+0100h         ; address to load boot block to

