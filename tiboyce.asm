#ifdef DEBUG
#ifndef CEMU
#define DBGNOSCALE
#endif
#endif

; Some configuration options
#ifndef SCANDELAY
#define SCANDELAY 12
#endif

#define FRAME_LENGTH (SCANDELAY*256*154)
#define TIMA_LENGTH (SCANDELAY*256*16/456)

#macro ACALL(address)
	call ArcCall
	.dw address+1
#endmacro

#macro AJUMP(address)
	call ArcJump
	.dw address+1
#endmacro

#macro APTR(address)
	call ArcPtr
	.dw address+1
#endmacro

#macro APTR_INLINE(address)
	ld hl,(ArcBase)
	ld de,address
	add hl,de
#endmacro

#macro APRINTF(text)
	APTR(text)
	push hl
	 ACALL(debug_printf)
	pop hl
#endmacro

MAGENTA = 10
OLIVE = 11
WHITE = 12
BLACK = 13
BLUE = 14

WHITE_BYTE = WHITE*$11
BLACK_BYTE = BLACK*$11
BLUE_BYTE = BLUE*$11

; Some standalone equates
_sprintf = $0000BC
_GetCSC = $02014C
_Mov9ToOP1 = $020320
_CmpPrgNamLen = $020504
_chkFindSym = $02050C
_InsertMem = $020514
_EnoughMem = $02051C
_CmpMemNeed = $020520
_CreatePVar4 = $020524
_CreatePVar3 = $020528
_DelVar = $020588
_DelMem = $020590
_ClrLCDFull = $020808
_HomeUp = $020828
_VPutSN = $020838
_RunIndicOff = $020848
_createAppVar = $021330
_DelVarArc = $021434
_Arc_Unarc = $021448
_DrawStatusBar = $021A3C
_ChkInRAM = $021F98
penCol = $D008D2
penRow = $D008D5
pTemp = $D0259A
progPtr = $D0259D
pixelShadow = $D031F6
userMem = $D1A881
vRam = $D40000
appVarObj = $15
tExtTok = $EF
tAsm84CeCmp = $7B

; 84+CE IO definitions
mpFlashWaitStates = $E00005

mpLcdTiming0 = $E30000
mpLcdTiming1 = $E30004
mpLcdTiming2 = $E30008
mpLcdBase = $E30010
mpLcdCtrl = $E30018
mpLcdImsc = $E3001C
mpLcdRis = $E30020
pLcdMis = $4024
mpLcdMis = $E30024
mpLcdIcr = $E30028
mpLcdPalette = $E30200
mpLcdCursorImg = $E30800

mpIntRawStatus = $F00000
mpIntEnable = $F00004
mpIntAcknowledge = $F00008
mpIntLatch = $F0000C
pIntMaskedStatus = $5014
mpIntMaskedStatus = $F00014

TMR_ENABLE = %00101001
mpTimer1Count = $F20000
mpTimer1Reset = $F20004
mpTimer1Match1 = $F20008
mpTimer1Match2 = $F2000C
mpTimer2Count = $F20010
mpTimer2Reset = $F20014
mpTimer2Match1 = $F20018
mpTimer2Match2 = $F2001C
mpTimer3Count = $F20020
mpTimer3Reset = $F20024
mpTimer3Match1 = $F20028
mpTimer3Match2 = $F2002C
mpTimerCtrl = $F20030
mpTimerIntStatus = $F20034

mpRtcSecondCount = $F30000

mpKeypadScanMode = $F50000
mpKeypadGrp0 = $F50010
mpKeypadGrp1 = $F50012
mpKeypadGrp2 = $F50014
mpKeypadGrp3 = $F50016
mpKeypadGrp4 = $F50018
mpKeypadGrp5 = $F5001A
mpKeypadGrp6 = $F5001C
mpKeypadGrp7 = $F5001E

#ifdef CEMU
mpCEmuDbg = $FB0000
#endif

mpZeroPage = $FF0000

;GB IO equates
ioregs = $ff00
P1 = $ff00
SB = $ff01
SC = $ff02
DIV = $ff04
TIMA = $ff05
TMA = $ff06
TAC = $ff07
IF = $ff0f

NR10 = $ff10
NR11 = $ff11
NR12 = $ff12
NR13 = $ff13
NR14 = $ff14

NR21 = $ff16
NR22 = $ff17
NR23 = $ff18
NR24 = $ff19

NR30 = $ff1a
NR31 = $ff1b
NR32 = $ff1c
NR33 = $ff1d
NR34 = $ff1e

NR41 = $ff20
NR42 = $ff21
NR43 = $ff22
NR44 = $ff23

NR50 = $ff24
NR51 = $ff25
NR52 = $ff26

WavePatternRAM = $ff30

LCDC = $ff40
STAT = $ff41
SCY = $ff42
SCX = $ff43
LY = $ff44
LYC = $ff45
DMA = $ff46
BGP = $ff47
OBP0 = $ff48
OBP1 = $ff49
WY = $ff4a
WX = $ff4b
IE = $ffff

; Memory areas used by the emulator
palettemem = mpLcdPalette
cursormem = mpLcdCursorImg

z80codebase = vRam
myz80stack = $FE00
scanlineLUT = pixelShadow
vram_tiles_start = scanlineLUT + (174*3)
vram_pixels_start = vram_tiles_start + $4000
vram_start = vram_pixels_start + $6000
digits = vram_start + $2000
fake_tile = $D0F900
fake_tilemap = $D0F940
wram_start = vram_start + $4000
memroutineLUT = vram_start + $6000
rombankLUT = memroutineLUT + $0200
rombankLUT_end = rombankLUT + (128*3)
hram_start = z80codebase + $FE00

vram_base = vram_start - $8000
wram_base = wram_start - $C000
hram_base = z80codebase

gb_frame_buffer_1 = vRam + (320*240)
text_frame_1 = gb_frame_buffer_1 + (160*150)
gb_frame_buffer_2 = gb_frame_buffer_1 + (160*240)
text_frame_2 = gb_frame_buffer_2 + (160*150)

recompile_struct = z80codebase + $010000
recompile_cache_end = gb_frame_buffer_1
	
	.db "TIBoyEXE",$01
	.dw program_size
	
	.org userMem
	
ArcCall:
	ex (sp),hl
	ld (ArcCallSMC),hl
	inc hl
	inc hl
	ex (sp),hl
	push hl
	 push de
ArcCallSMC = $+1
	  ld hl,(0)
	  dec.s hl
ArcBase = $+1
	  ld de,0
	  add hl,de
	 pop de
	 ex (sp),hl
	 ret
	 
ArcJump:
	ex (sp),hl
	push de
	 ld de,(hl)
	 dec.s de
	 ld hl,(ArcBase)
	 add hl,de
	pop de
	ex (sp),hl
	ret
	
ArcPtr:
	pop hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	push hl
	 dec.s de
	 ld hl,(ArcBase)
	 add hl,de
	ret
	
memcmp:
	ld a,(de)
	inc de
	cpi
	ret nz
	ret po
	jr memcmp
	
get_current_menu_selection:
	ld hl,MenuList
current_menu = $+1
	ld c,0
	ld b,2
	mlt bc
	add hl,bc
	ld bc,(ArcBase)
	add hl,bc
	ld hl,(hl)
	dec.s hl
	add hl,bc
current_menu_selection = $+1
	ld c,0
	ret
	
SetStringColor:
	inc a
	ld (PutChar_ColorSMC1),a
	ld (PutChar_ColorSMC2),a
	ret
	
	; A = character to display
	; (cursorRow), (cursorCol) is location to display
PutChar:
	or a
	jr z,_
	sub ' '
	jr nc,_
	AJUMP(PutNewLine)
_
	ld c,a
	ld b,10
	mlt bc
	APTR_INLINE(font)
	add hl,bc
	ex de,hl
	
	ld hl,(cursorCol)
	ld a,l
	cp 40
	ret nc
	ld l,160
	mlt hl
	ld bc,(current_buffer)
	add hl,bc
	ld c,a
	ld b,4
	mlt bc
	add hl,bc
	
	ld b,10
PutCharRowLoop:
	push bc
	 ld a,(de)
	 inc de
	 ld b,4
	 ld c,a
PutCharPixelLoop:
	 sla c
	 sbc a,a
PutChar_ColorSMC1 = $+1
	 or WHITE+1
	 dec a
	 ld (hl),a
	 sla c
	 sbc a,a
PutChar_ColorSMC2 = $+1
	 or WHITE+1
	 dec a
	 rld
	 inc hl
	 djnz PutCharPixelLoop
	 ld c,160-4
	 add hl,bc
	pop bc
	djnz PutCharRowLoop
	ld hl,cursorCol
	inc (hl)
	ret
	
menuFrame:
	.dl 0
menuSelection:
	.dl 0
lastROM:
	.dl 0
	
ROMName:
	.db appVarObj
	.block 9
	
MetaHeader:
	.db "TIBOYCE",0
	
saveSP:
	.dl 0
rom_start:
	.dl 0
rom_bank_base:
	.dl 0
cram_start:
	.dl 0
cram_bank_base:
	.dl 0
mbc:
	.db 0
	
skippable_frames:
	.db 1
current_buffer:
	.dl 0
	
cursorCol:
	.db 0
cursorRow:
	.db 0
text_buffer:
	.block 42
	
current_item_ptr:
	.dl 0
current_description:
	.dl 0
main_menu_selection:
	.db 1
current_state:
	.db 0

FrameskipValue:
	.db 2
	
OptionConfig:
FrameskipType:
	.db 1
FPSDisplay:
	.db 0
AutoArchive:
	.db 1
	
KeyConfig:
	.db 3,2,4,1,54,48,40,55,15
	
	#include "opgen.asm"
	#include "ophandler.asm"
program_end:
ram_size:
program_size = program_end - userMem
	
	.echo "User RAM code size: ", program_size
	
	#include "setup.asm"
	#include "text.asm"
	#include "menu.asm"
	#include "z80mode.asm"
	#include "render.asm"

	.echo "Total size: ", program_size + $