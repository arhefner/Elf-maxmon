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

findtkn:    equ   RAMBASE+0030h         ; jump vector for f_findtkn
idnum:      equ   RAMBASE+0033h         ; jump vector for f_idnum
devbits:    equ   RAMBASE+0036h         ; f_getdev device present result
clkfreq:    equ   RAMBASE+0038h         ; processor clock frequency in khz
lastram:    equ   RAMBASE+003ah         ; f_freemem last ram address result
type:       equ   RAMBASE+003ch         ; RAM vector for console output
read:       equ   RAMBASE+003fh         ; RAM vector for console input

buffer:     equ   RAMBASE+0048h         ; buffer for I/O
bufend:     equ   RAMBASE+007fh
buflen:     equ   bufend-buffer+1

savexp:     equ   RAMBASE+0080h
saved:      equ   RAMBASE+0081h
savedf:     equ   RAMBASE+0082h
regs:       equ   RAMBASE+0083h         ; memory for register save

stack:      equ   RAMBASE+00ffh         ; top of temporary booting stack

bootpg:     equ   RAMBASE+0100h         ; address to load boot block to

            ; Elf/OS Kernel Variables

o_wrmboot:  equ   0303h                 ; kernel warm-boot reinitialization
k_clkfreq:  equ   0470h                 ; processor clock frequency in khz
