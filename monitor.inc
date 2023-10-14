; Define address for standard monitor vectors
#ifndef MAX_MON
#define MAX_MON 0f7f7h
#endif

m_break:    equ   (MAX_MON+00h)         ; trigger a monitor breakpoint
m_load:     equ   (MAX_MON+03h)         ; load a file over serial
m_save:     equ   (MAX_MON+06h)         ; save a file over serial
