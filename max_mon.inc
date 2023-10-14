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

            ; Elf/OS Kernel Variables

o_wrmboot:  equ   0303h                 ; kernel warm-boot reinitialization
k_clkfreq:  equ   0470h                 ; processor clock frequency in khz
