#ifdef DEBUG
#ifndef CEMU
#define DBGNOSCALE
#endif
#endif

; The length of a GB scanline, proportional to host CPU clock speed
#ifndef SCANDELAY
#define SCANDELAY 12
#endif

; The length of an entire GB frame in host CPU cycles
#define FRAME_LENGTH (SCANDELAY*256*154)
; The length of the shortest GB timer tick (16 cycles) in host CPU cycles
#define TIMA_LENGTH (SCANDELAY*256*16/456)

; A call to a routine located in the archived appvar.
; Destroys flags before entry to routine.
#macro ACALL(address)
	call ArcCall
	.dw address+1
#endmacro

; A jump to a label located in the archived appvar.
; Destroys flags.
#macro AJUMP(address)
	call ArcJump
	.dw address+1
#endmacro

; Puts the pointer to a label in the archived appvar into HL.
; Destroys DE and flags.
#macro APTR(address)
	call ArcPtr
	.dw address+1
#endmacro

; Inline version of APTR, with the same effects.
#macro APTR_INLINE(address)
	ld hl,(ArcBase)
	ld de,address
	add hl,de
#endmacro

; Debug-prints using a format string in the archived appvar.
; The variable argument list must first be pushed to the stack.
#macro APRINTF(text)
	APTR(text)
	push hl
	 ACALL(debug_printf)
	pop hl
#endmacro

; Constant color palette entries
MAGENTA = 10
OLIVE = 11
WHITE = 12
BLACK = 13
BLUE = 14

; Paletted colors doubled into two pixels
WHITE_BYTE = WHITE*$11
BLACK_BYTE = BLACK*$11
BLUE_BYTE = BLUE*$11

; System calls used
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
_VPutS = $020834
_VPutSN = $020838
_RunIndicOff = $020848
_createAppVar = $021330
_DelVarArc = $021434
_Arc_Unarc = $021448
_DrawStatusBar = $021A3C
_ChkInRAM = $021F98

; RAM addresses used
ramStart = $D00000
penCol = $D008D2
penRow = $D008D5
pTemp = $D0259A
progPtr = $D0259D
drawFGColor = $D026AC
pixelShadow = $D031F6
userMem = $D1A881
vRam = $D40000

; Tokens used
appVarObj = $15
tExtTok = $EF
tAsm84CeCmp = $7B

; OS flags used
graphFlags = $03
graphDraw = 0		;0=graph is valid, 1=redraw graph(dirty)

textFlags = $05		;Text output flags
textInverse = 3		;1=display inverse bit-map

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

; The 16-bit Z80 address space starts here.
z80codebase = vRam
; The bottom of the Z80 stack. Grows down from the Game Boy HRAM start.
myz80stack = $FE00

; A list of scanline start addresses for sprites. 174 pointers in size.
; Bit 0 set if disabled. The top/bottom 15 entries are permanently disabled.
scanlineLUT = pixelShadow

; Preprocessed Game Boy tilemap entries. 16KB in size.
; Each tile entry is a 2-byte offset of the pixel data from vram_pixels_start.
; Every row of 32 tiles is duplicated into 64, to facilitate wraparound.
; In addition, each duplicated row is stored twice with different tilesets.
; So, each GB tilemap row takes a total of 256 bytes here.
; Buffer must be 256-byte aligned and contained within one 64KB-aligned block.
vram_tiles_start = scanlineLUT + (174*3)

; Preprocessed Game Boy tile pixel entries. 24KB in size.
; Each tile is converted into one byte per pixel, for 64 bytes per tile.
; Buffer must be 256-byte aligned.
vram_pixels_start = vram_tiles_start + $4000

; Start of Game Boy VRAM. 8KB in size.
vram_start = vram_pixels_start + $6000

; Preconverted digit pixels for displaying FPS quickly. 400 bytes in size.
digits = vram_start + $2000

; A fake tile filled with Game Boy color 0. 64 bytes in size.
; Used when BG tilemap is disabled.
fake_tile = $D0F900
; A fake tilemap row pointing to fake_tile. 42 bytes in size.
; Used when BG tilemap is disabled.
fake_tilemap = $D0F940

; Start of Game Boy WRAM. 8KB in size.
wram_start = vram_start + $4000

; Start of Z80 memory routine lookup table. 512 bytes in size.
; The first 256 bytes are the routine LSBs and the next 256 are the MSBs.
; A null address means that the routine has not yet been generated.
; The lookup table is indexed uniquely by (mem_region*32)+access_type.
; Buffer must be 256-byte aligned and contained within one 64KB-aligned block.
memroutineLUT = vram_start + $6000

; Start of the ROM bank lookup table. 128 pointers in size.
; Stores the base address of each ROM bank (or mirror), minus $4000.
; The pre-subtracted $4000 means that the memory can be indexed by GB address.
; This buffer is also used to cache the pointers to appvars in the ROM list.
rombankLUT = memroutineLUT + $0200
rombankLUT_end = rombankLUT + (128*3)

; Start of Game Boy HRAM region. 512 bytes in size, includes OAM and MMIO.
hram_start = z80codebase + $FE00

; Base address of VRAM, can be indexed directly by Game Boy address.
vram_base = vram_start - $8000
; Base address of WRAM, can be indexed directly by Game Boy address.
wram_base = wram_start - $C000
; Base address of HRAM, can be indexed directly by Game Boy address.
hram_base = z80codebase

; Start of first 4bpp frame buffer. 160*240 bytes in size.
gb_frame_buffer_1 = vRam + (320*240)
; Start of first debug text buffer. 160*90 bytes in size.
text_frame_1 = gb_frame_buffer_1 + (160*150)
; Start of second 4bpp frame buffer. 160*240 bytes in size.
gb_frame_buffer_2 = gb_frame_buffer_1 + (160*240)
; Start of second debug text buffer. 160*90 bytes in size.
text_frame_2 = gb_frame_buffer_2 + (160*150)

; Start of structure array keeping track of recompiled code blocks. 11KB max.
; Grows forward into the recompiled code mapping cache (see below).
; Each entry is 8 bytes in size, and contains the following members:
;   +0: A 16-bit pointer to the start of the recompiled Z80 code block.
;   +2: A 24-bit pointer to the first GB source opcode.
;   +5: A 24-bit pointer to the start of the last GB source opcode.
; The address of the first unused entry is stored in (recompile_struct_end).
; The blocks are sorted in ascending order by Z80 block start address.
; Note: The first unused entry always contains a *24-bit* pointer to the next
;       available block start address. The upper 8 bits will be overwritten.
; Buffer must be 64KB-aligned, typically located directly after Z80 code space.
recompile_struct = z80codebase + $010000

; End of array caching mappings of GB addresses to recompiled code. 11KB max.
; Grows backward into the recompiled code block information (see above).
; Each entry is 5 bytes in size, and contains the following members:
;   +0: 24-bit pointer to a Game Boy opcode.
;   +3: 16-bit pointer to a recompiled code pointer. May be inside a block.
; The start of the array is stored in (recompile_cache).
; The array is sorted in ascending order by Game Boy opcode address.
; Lookup is O(log n) on number of entries, and insertion is O(n) plus mapping.
; Only addresses reached via RET (when callstack fails) or JP HL use the cache.
; Buffer end must be 256-byte aligned.
recompile_cache_end = gb_frame_buffer_1
	
	.db "TIBoyEXE",$01
	.dw program_size
	
	.org userMem
	
; Calls a routine located in the archived appvar.
; The 16-bit offset (plus 1) is stored at the return address.
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
	 
; Jumps to an address located in the archived appvar.
; The 16-bit offset (plus 1) is stored at the return address.
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
	
; Puts a pointer located in the archived appvar in HL.
; The 16-bit offset (plus 1) is stored at the return address.
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
	
; Compares the buffers at HL and DE, with size BC. Returns Z if equal.
memcmp:
	ld a,(de)
	inc de
	cpi
	ret nz
	ret po
	jr memcmp
	
; Gets a pointer to the current menu in HL, and the current selection in C.
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
	
; Sets the current string color.
SetStringColor:
	inc a
	ld (PutChar_ColorSMC1),a
	ld (PutChar_ColorSMC2),a
	ret
	
; Renders the character in A on the current buffer at (cursorRow), (cursorCol).
PutChar:
	; Convert to font index. 0 is treated as a space, and 1-31 as newline.
	or a
	jr z,_
	sub ' '
	jr nc,_
	AJUMP(PutNewLine)
_
	; Put pointer to character data in appvar in DE.
	ld c,a
	ld b,10
	mlt bc
	APTR_INLINE(font)
	add hl,bc
	ex de,hl
	
	; Get pointer to buffer output location in HL.
	ld hl,(cursorCol)
	; Refuse to draw past the right side of the screen.
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
	
	; Draw 10 rows of pixels.
	ld b,10
PutCharRowLoop:
	push bc
	 ; Get the bitmap for the current row in C.
	 ld a,(de)
	 inc de
	 ld c,a
	 ld b,4
PutCharPixelLoop:
	 ; Render 2 pixels to the framebuffer per iteration.
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
	 ; Advance output pointer to the next row.
	 ld c,160-4
	 add hl,bc
	pop bc
	djnz PutCharRowLoop
	; Increment the cursor column.
	ld hl,cursorCol
	inc (hl)
	ret
	
; The first ROM in the current list frame.
menuFrame:
	.dl 0
; The currently selected ROM.
menuSelection:
	.dl 0
; The last selected ROM.
menuLastSelection:
	.dl 0
; The end of the list of discovered ROMs.
ROMListEnd:
	.dl 0
	
; The name of the currently loaded ROM.
ROMName:
	.db appVarObj
	.block 9
; The ROM appvar magic header.
MetaHeader:
	.db "TIBOYCE",0
	
; The backup of SP before beginning emulation.
saveSP:
	.dl 0
; The start address of Game Boy ROM page 0.
rom_start:
	.dl 0
; The address of the currently banked ROM page, minus $4000.
; Can be indexed directly by the Game Boy address.
rom_bank_base:
	.dl 0
; The start address of Game Boy cartridge RAM.
cram_start:
	.dl 0
; The address of the currently banked RAM page, minus $A000.
; Can be indexed directly by the Game Boy address.
cram_bank_base:
	.dl 0
; The cartridge's Memory Bank Controller type.
mbc:
	.db 0
	
; The number of frames left to skip, plus 1.
skippable_frames:
	.db 1
; The current buffer to render to.
current_buffer:
	.dl 0
; The default palette.
default_palette:
	.db 0
	
; The current text output column, in characters (0-39).
cursorCol:
	.db 0
; The current text output row, in pixels (0-230).
cursorRow:
	.db 0
; The output buffer for printf, large enough for a single row of text.
text_buffer:
	.block 42
	
; The pointer to the currently selected menu item.
current_item_ptr:
	.dl 0
; The pointer to the active ROM description.
current_description:
	.dl 0
; A backup of the selected main menu option.
main_menu_selection:
	.db 1
; The currently chosen save state index.
current_state:
	.db 0
	
	
; Current palette color sources (must be in this order).
; Correspond to colors 0-3 in sequence.
palette_bg_colors:
	.dw $0421 * 31 + $8000
	.dw $0421 * 21 + $8000
	.dw $0421 * 10
	.dw $0421 * 0
palette_obj0_colors:
	.dw $0421 * 31 + $8000
	.dw $0421 * 21 + $8000
	.dw $0421 * 10
	.dw $0421 * 0
palette_obj1_colors:
	.dw $0421 * 31 + $8000
	.dw $0421 * 21 + $8000
	.dw $0421 * 10
	.dw $0421 * 0
	
	
; Active configuration info:
	
; The currently chosen frameskip value.
FrameskipValue:
	.db 2
	
OptionConfig:
; Frameskip type (0=Manual, 1=Auto, 2=Off)
FrameskipType:
	.db 1
; FPS display (0=Off, 1=On)
FPSDisplay:
	.db 0
; Auto archive (0=Off, 1=On)
AutoArchive:
	.db 1
; Palette selection (0=Default, 1...=Manual)
PaletteSelection:
	.db 0
	
; Key configuration. Each is a GetCSC scan code.
KeyConfig:
	.db 3,2,4,1,54,48,40,55,15
	
; These files are loaded into RAM.
	#include "jit.asm"
	#include "decode.asm"
	#include "ophandler.asm"
	#include "vblank.asm"
	#include "waitloop.asm"
	
; The RAM program ends here.
program_end:
program_size = program_end - userMem
	.echo "User RAM code size: ", program_size

; The size of the inserted cartridge RAM is located at the end of the program.
ram_size:
	
; These files remain in the archived appvar.
	#include "setup.asm"
	#include "text.asm"
	#include "menu.asm"
	#include "z80mode.asm"
	#include "render.asm"

	.echo "Total size: ", program_size + $