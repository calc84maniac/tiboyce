#define CALL_STACK_DEPTH 32
#define CALL_STACK_ENTRY_SIZE_Z80 6
#define CALL_STACK_ENTRY_SIZE_ADL 3

#define ERROR_CATCHER (Z80Error << 8) | $C3

; Gets the 24-bit base pointer for a given Game Boy address.
; The base plus the address can be used to directly access GB memory.
;
; Inputs:  DE = GB address
; Outputs: HL = base pointer
; Destroys: F
#macro GET_BASE_ADDR_NO_ASSERT
	ld hl,mem_region_lut
	ld l,d
	ld l,(hl)
	dec h
	ld hl,(hl)
#endmacro

#macro GET_BASE_ADDR_FAST
#ifdef DEBUG
	; Assert high byte of DE is 0
	ld hl,$FF0000
	add hl,de
	jr c,$
#endif
	GET_BASE_ADDR_NO_ASSERT
#endmacro

#macro ASSERT_NC
#ifdef DEBUG
	jr c,$
#endif
#endmacro

#macro ASSERT_C
#ifdef DEBUG
	jr nc,$
#endif
#endmacro

; A call to a routine located in the archived appvar.
; Destroys flags before entry to routine.
#macro ACALL_SAFERET(address)
	call ArcCallArcReturn
	.dw address+1
#endmacro

; A call to a routine located in the archived appvar.
; Destroys flags before entry to routine. Nothing above this may archive.
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
; Destroys DE and flags. Clears carry flag.
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

#macro SPI_START
	#define SPI_BIT $80
	#define SPI_VALUE $00
#endmacro
	
#macro SPI_CMD(cmd)
	.db ((cmd * SPI_BIT) >> 8) | SPI_VALUE
	#define SPI_VALUE eval((cmd * SPI_BIT) & $FF)
	#if SPI_BIT == $01
	SPI_END
	SPI_START
	#else
	#define SPI_BIT eval(SPI_BIT >> 1)
	#endif
#endmacro

#macro SPI_PARAM(param)
	.db ((param * SPI_BIT) >> 8) | (SPI_VALUE | SPI_BIT)
	#define SPI_VALUE eval((param * SPI_BIT) & $FF)
	#if SPI_BIT == $01
	SPI_END
	SPI_START
	#else
	#define SPI_BIT eval(SPI_BIT >> 1)
	#endif
#endmacro

#macro SPI_PARAM16(param)
	SPI_PARAM(param >> 8)
	SPI_PARAM(param & $FF)
#endmacro

#macro SPI_END
	#if SPI_BIT != $80
	.db SPI_VALUE
	#endif
	#undef SPI_VALUE
	#undef SPI_BIT
#endmacro

; State variable indices
STATE_SYSTEM_TYPE = 0
STATE_INTERRUPTS = 1
STATE_REG_AF = 2
STATE_REG_BC = 4
STATE_REG_DE = 6
STATE_REG_HL = 8
STATE_REG_SP = 10
STATE_REG_PC = 12
STATE_FRAME_COUNTER = 14
STATE_SERIAL_COUNTER = 16
STATE_DIV_COUNTER = 18
STATE_ROM_BANK = 20
STATE_RAM_BANK = 21
STATE_MBC_MODE = 22
STATE_CPU_MODE = 23
STATE_END = 24

; Constant color palette entries
BLUE = 3
MAGENTA = 4
OLIVE = 5
BLACK = 6
WHITE = 7
GRAY = 8
; Palette entries representing the raw BGP colors. Must precede OBP0 colors.
BG_COLOR_0 = 9
BG_COLOR_1 = 10
BG_COLOR_2 = 11
BG_COLOR_3 = 12
; Palette entries representing every possible combination of three OBJ colors.
; The data is overlapped such that a unique sequence of three colors begins
; at each entry. Note that this means each table is (64 + 2) entries large.
; Additionally, the original sequence of colors 0, 1, 2, 3 is present starting
; at the offset corresponding to palette %100100, which is $0D.
OBP0_COLORS_START = 13
OBP0_ORIG_COLORS = OBP0_COLORS_START+$0D
OBP1_COLORS_START = OBP0_COLORS_START + (64 + 2)
OBP1_ORIG_COLORS = OBP1_COLORS_START+$0D

; System calls used
_sprintf = $0000BC
_GetFieldSizeFromType = $00030C
_FindField = $000314
_boot_InitializeHardware = $000384
_OSHeader = $020000
_Mov9ToOP1 = $020320
_MemChk = $0204FC
_CmpPrgNamLen = $020504
_chkFindSym = $02050C
_InsertMem = $020514
_CreatePVar4 = $020524
_DelMem = $020590
_ErrUndefined = $020764
_JError = $020790
_PushErrorHandler = $020798
_PopErrorHandler = $02079C
_ClrLCDFull = $020808
_HomeUp = $020828
_RunIndicOff = $020848
_DelVarArc = $021434
_Arc_Unarc = $021448
_DrawStatusBar = $021A3C
_DivHLByA = $021D90
_ChkInRam = $021F98
_FindFreeArcSpot = $022078

; RAM addresses used
ramStart = $D00000
flags = $D00080
brightness = $D0058F
asm_data_ptr1 = $D0067E
penCol = $D008D2
penRow = $D008D5
asm_prgm_size = $D0118C
tSymPtr1 = $D0257B
pTemp = $D0259A
progPtr = $D0259D
drawFGColor = $D026AC
pixelShadow = $D031F6 ; Start of SafeRAM
usbArea	= $D13FD8 ; End of SafeRAM
osYear = $D177CF
osDay = $D177D8
osMonth = $D177DB
userMem = $D1A881
vRam = $D40000

; Tokens/characters used
appVarObj = $15
tExtTok = $EF
tAsm84CeCmp = $7B
LlBrack = $C1

; OS flags used
graphFlags = $03
graphDraw = 0		;0=graph is valid, 1=redraw graph(dirty)

textFlags = $05		;Text output flags
textInverse = 3		;1=display inverse bit-map

; 84+CE IO definitions
mpFlashWaitStates = $E00005

mpShaData = $E10010

mpLcdTiming0 = $E30000
mpLcdTiming1 = $E30004
mpLcdTiming2 = $E30008
mpLcdBase = $E30010
mpLcdCtrl = $E30018
mpLcdImsc = $E3001C
mpLcdRis = $E30020
mpLcdMis = $E30024
mpLcdIcr = $E30028
mpLcdCurr = $E3002C
mpLcdPalette = $E30200
mpLcdCursorImg = $E30800

mpIntRawStatus = $F00000
mpIntEnable = $F00004
mpIntAcknowledge = $F00008
mpIntLatch = $F0000C
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
mpRtcMinuteCount = $F30004
mpRtcHourCount = $F30008
mpRtcDayCount = $F3000C
mpRtcCtrl = $F30020
mpRtcIntStatus = $F30034

mpKeypadScanMode = $F50000
mpKeypadGrp0 = $F50010
mpKeypadGrp1 = $F50012
mpKeypadGrp2 = $F50014
mpKeypadGrp3 = $F50016
mpKeypadGrp4 = $F50018
mpKeypadGrp5 = $F5001A
mpKeypadGrp6 = $F5001C
mpKeypadGrp7 = $F5001E

mpBlLevel = $F60024

mpSpiConfig = $F80000
mpSpiDivider = $F80004
mpSpiTransfer = $F80008
mpSpiStatus = $F8000C
mpSpiUnknown1 = $F80010
mpSpiUnknown2 = $F80014
mpSpiFifo = $F80018

#ifdef DEBUG
mpCEmuDbg = $FB0000
#endif

mpZeroPage = $FF0000

;GB IO equates
MODE_0_CYCLES = 51
MODE_2_CYCLES = 20
MODE_3_CYCLES = 43
CYCLES_PER_SCANLINE = MODE_2_CYCLES + MODE_3_CYCLES + MODE_0_CYCLES
SCANLINES_PER_FRAME = 154
CYCLES_PER_FRAME = CYCLES_PER_SCANLINE * SCANLINES_PER_FRAME

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
; The lower bound of the call stack.
call_stack_lower_bound = myz80stack - 4 - (CALL_STACK_DEPTH * CALL_STACK_ENTRY_SIZE_Z80)
; The flags translation LUT
flags_lut = myz80stack - 512
; The end of the memory routines.
memroutine_end = flags_lut - 3

; The bottom of the ADL stack. Grows down from the end of SafeRAM.
myADLstack = usbArea

; Preprocessed Game Boy tilemap entries. 16KB in size.
; Each tile entry is a 2-byte offset of the pixel data from vram_pixels_start.
; Every row of 32 tiles is duplicated into 64, to facilitate wraparound.
; In addition, each duplicated row is stored twice with different tilesets.
; So, each GB tilemap row takes a total of 256 bytes here.
; Buffer must be 256-byte aligned and contained within one 64KB-aligned block.
vram_tiles_start = (pixelShadow | $FF) + 1
decompress_buffer = vram_tiles_start

; Preprocessed Game Boy tile pixel entries. 24KB in size.
; Each tile is converted into one byte per pixel, for 64 bytes per tile.
; Buffer must be 256-byte aligned.
vram_pixels_start = vram_tiles_start + $4000
; The mini frame backup in the menu is temporarily stored in this area.
; 160 * 144 = 22.5 KB in size.
mini_frame_backup = vram_pixels_start

; Start of Z80 memory routine lookup table. 512 bytes in size.
; The first 256 bytes are the routine LSBs and the next 256 are the MSBs.
; A null address means that the routine has not yet been generated.
; The lookup table is indexed uniquely by (mem_region*32)+access_type.
; Buffer must be 256-byte aligned and contained within one 64KB-aligned block.
memroutineLUT = vram_pixels_start + $6000

; Start of recompiler struct index lookup table. 512 bytes in size.
; The first 256 bytes are the LSBs and the next 256 are the MSBs.
; The lookup table is indexed by the high byte of a JIT address,
; and each pointer gives the last struct entry touching that range.
recompile_index_LUT = memroutineLUT + $0200

; Start of recompiler cached jump index lookup table. 512 bytes in size.
; The first 256 bytes are the LSBs and the next 256 are the MSBs.
; The lookup table is indexed by the low byte of a GB address,
; and each pointer gives the first cache entry corresponding to that LSB.
recompile_cache_LUT = recompile_index_LUT + $0200

; A lookup table for converting BG palettes to raw colors. 256 bytes in size.
; Must be 256-byte aligned.
convert_palette_LUT = recompile_cache_LUT + $0200

; Start of the ROM bank lookup table. 256 pointers in size.
; Stores the base address of each ROM bank (or mirror), minus $4000.
; The pre-subtracted $4000 means that the memory can be indexed by GB address.
; This buffer is also used to cache the pointers to appvars in the ROM list.
rombankLUT = convert_palette_LUT + 256
rombankLUT_end = rombankLUT + (256*3)

; A table with the largest Y position of a sprite using the corresponding tile,
; or 0 if the tile is not used by any onscreen sprite.
; Must be 256-byte aligned.
oam_tile_usage_lut = rombankLUT_end

; Specifies offsets into a buffer of pixel data corresponding to the
; input 2bpp pixel data. Note that the input has the high palette bits
; grouped in the high nibble, and the low palette bits in the low nibble.
; Must be 256-byte aligned.
overlapped_pixel_index_lut = oam_tile_usage_lut + 256

; A table representing every possible combination of four 2bpp pixels.
; The data is overlapped such that a unique sequence of four pixels begins
; at each byte. Note that this means the table is 256 + 3 bytes large.
; Must be 256-byte aligned and directly follow the LUT.
overlapped_pixel_data = overlapped_pixel_index_lut + 256

; Preconverted digit pixels for displaying FPS quickly. 24 bytes per character, 264 bytes total.
; Must be 8-byte aligned.
digits = (((overlapped_pixel_data + (256 + 3)) - 1) | 7) + 1

; Temporary backup for 16 palette entries. 32 bytes in size.
palette_backup = digits + 264

; Queue for BGP writes during a frame. 192+1 bytes in size.
; Stored in a compressed format, with either:
;   Byte 0 = Number of scanlines, Byte 1 = BGP value for all scanlines
;   Byte 0 = Number of scanlines (N) + 144, Bytes 1-N = BGP values per scanline
; Must end at a 256-byte aligned address.
BGP_write_queue = (((palette_backup + 32) + 192) | 255) - 192

; Tracks the frequency of each BGP value as they are queued.
; The BGP value with the highest frequency will use the native palette.
; Must be 256-byte aligned and directly follow the queue.
BGP_frequencies = BGP_write_queue + 192 + 1

; Specifies indices into an array of color data corresponding to the
; input 2bpp palette data. Note that the input is a BGP, OBP0, or OBJ1
; value, and identifies three colors corresponding to the upper 6 bits.
; Must be 256-byte aligned (and currently, follow BGP_frequencies).
overlapped_palette_index_lut = BGP_frequencies + 256

; Table representing every possible combination of three BGP colors.
; The data is overlapped such that a unique sequence of three colors begins
; at each word. Note that this means the table is (64 + 2) * 2 bytes large.
; Additionally, the original sequence of colors 0, 1, 2, 3 is present starting
; at the index corresponding to palette %100100, which is $0D.
; This table must be 256-byte aligned.
overlapped_bg_palette_colors = overlapped_palette_index_lut + 256
bg_palette_colors = overlapped_bg_palette_colors + ($0D*2)

; A list of scanline start addresses. 144*2 pointers in size.
scanlineLUT_1 = overlapped_bg_palette_colors + ((64 + 2) * 2)
scanlineLUT_2 = scanlineLUT_1 + (144*3)

; A list of VAT entries for found ROM files. 256 pointers in size.
romListStart = scanlineLUT_2 + (144*3)

; A fake tile filled with Game Boy color 0. 64 bytes in size.
; Used when BG tilemap is disabled.
fake_tile = $D0F900
; A fake tilemap row pointing to fake_tile. 42 bytes in size.
; Used when BG tilemap is disabled.
fake_tilemap = $D0F940

; Start of Game Boy HRAM region. 512 bytes in size, includes OAM and MMIO.
hram_start = z80codebase + $FE00
; Start of state saving/loading area.
state_start = hram_start + $00A0
state_size = $60
; Base address of HRAM, can be indexed directly by Game Boy address.
hram_base = z80codebase

; Start of menu 8bpp frame buffer. 320*240 bytes in size.
menu_frame_buffer = vRam + (320*240)

; Start of first 8bpp frame buffer. 160*240 bytes in size.
gb_frame_buffer_1 = vRam + (320*240)
; Start of second 8bpp frame buffer. 160*240 bytes in size.
gb_frame_buffer_2 = gb_frame_buffer_1 + (160*240)

; Start of structure array keeping track of recompiled code blocks. 11KB max.
; Grows forward into the recompiled code mapping cache (see below).
; Each entry is 8 bytes in size, and contains the following members:
;   +0: A 16-bit pointer to the start of the recompiled Z80 code block.
;   +2: A 24-bit banked GB address of the first source opcode.
;   +5: The 8-bit size of the GB opcode block.
;   +7: The 8-bit total cycle count of the block.
; The address of the first unused entry is stored in (recompile_struct_end).
; The blocks are sorted in ascending order by Z80 block start address.
; Note: The first unused entry always contains a *24-bit* pointer to the next
;       available block start address. The upper 8 bits will be overwritten.
; Buffer must be 64KB-aligned, typically located directly after Z80 code space.
recompile_struct = z80codebase + $010000

; End of array caching mappings of GB addresses to recompiled code. 11KB max.
; Grows backward into the recompiled code block information (see above).
; Each entry is 5 bytes in size, and contains the following members:
;   +0: 16-bit pointer to recompiled code. May be inside a non-RAM-based block.
;   +2: 8-bit clock cycle index within the block.
;   +3: Upper 16 bits of the 24-bit banked Game Boy address.
; The start of the array is stored in (recompile_cache).
; The array is logically separated into ranges, each range corresponding to
; the LSB of the contained GB addresses, in decreasing order ($FF to $00).
; Range bounds are tracked in the recompile_cache_LUT and used for fast lookup.
; Lookup is O(n) on number of entries with a given LSB. Insertion is O(n) on
; number of total entries, plus the address lookup and updating LSB ranges.
; Only addresses reached via RET (when callstack fails) or JP HL use the cache.
; Additionally, jumps from RAM-based GB code use the cache to speed up SMC,
; and jumps/calls to banked regions use the cache upon a bank mismatch.
; Buffer end must be 256-byte aligned.
recompile_cache_end = gb_frame_buffer_1

	.db "TIBoyEXE",$01
	.dw program_size
header_size = $
	
	.org userMem
	
; Calls a routine located in the archived appvar.
; The 16-bit offset (plus 1) is stored at the return address.
ArcCall:
	ex (sp),hl
	ld (ArcCallSMC),hl
	inc hl
	inc hl
	ex (sp),hl
ArcCallEntry:
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
	 
; Calls a routine located in the archived appvar. Returns to the appvar.
; The 16-bit offset (plus 1) is stored at the return address.
ArcCallArcReturn:
	ex (sp),hl
	ld (ArcCallSMC),hl
	inc hl
	inc hl
	push de
	 ld de,(ArcBase)
	 or a
	 sbc hl,de
	pop de
	ex (sp),hl
	call ArcCallEntry
	ex (sp),hl
	push af
	 push de
	  ld de,(ArcBase)
	  add hl,de
	 pop de
	pop af
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
	
; Archives or unarchives a variable. Updates appvar in case of garbage collect.
; Returns carry set on failure.
Arc_Unarc_Safe:
	call _chkFindSym
	ret c
	call _ChkInRam
	jr nz,++_
	; If archiving, check if there is a free spot in archive
	ex de,hl
	ld hl,(hl)
	ld a,c
	add a,12
	ld c,a
	ld b,0
	add.s hl,bc
	jr c,_
	push hl
	pop bc
	call _FindFreeArcSpot
	jr nz,++_
_
	; No free spot, so prepare for Garbage Collect message
	ACALL(RestoreHomeScreen)
_
	push ix
	 ld ix,(tSymPtr1)
	 ld hl,(ix-7)
	 ld h,(ix-4)
	 ld l,(ix-3)
	pop ix
	push hl
	 ld hl,Arc_Unarc_ErrorHandler
	 call _PushErrorHandler
	 call _Arc_Unarc
	 call _PopErrorHandler
	 .db $3E	; LD A,$AF
Arc_Unarc_ErrorHandler:
	 xor a
	 push af
	  ld hl,SelfName
	  call _Mov9ToOP1
	  call _chkFindSym
	  jr c,EpicFailure
	  ld (tSymPtr1),hl
	 pop af
	pop hl
	ex de,hl
	or a
	sbc hl,de
	ex de,hl
	ld hl,(ArcBase)
	add hl,de
	ld (ArcBase),hl
	pop hl
	add hl,de
	cp 1
CallHL:
	jp (hl)
	
	; Set ArcBase to -1 so the error handler won't try to call the appvar
EpicFailure:
	sbc hl,hl
	ld (ArcBase),hl
	jp _ErrUndefined
	
GlobalErrorHandler:
	push af
	 ; Only restore the homescreen if ArcBase is valid.
	 ; This may be invalid if a Garbage Collect caused the
	 ; executable AppVar to disappear, but in that case the
	 ; homescreen has already been restored.
	 ld hl,(ArcBase)
	 ld de,RestoreHomeScreen
	 add hl,de
	 call nc,CallHL
	pop af
	res 7,a
	jp _JError
	
DelVarByName:
	call _Mov9ToOP1
	call _chkFindSym
	jp nc,_DelVarArc
	ret
	
	; Input: B=index
GetRomDescriptionByIndex:
	ld iy,romListStart
	ld c,3
	mlt bc
	add iy,bc
	
	; Input: IY=ROM list entry
	; Output: HL=Description string start
	;         IX=ROM pointer
	;         BC=12
	;         Carry is reset
	;         A, DE, IY are preserved
GetRomDescription:
	ld ix,(iy)
GetRomDescriptionFromVAT:
	ld hl,(ix)
	ld h,(ix+3)
	ld l,(ix+4)
	
	; Check if in RAM, also decrementing by 1 if in RAM
	ld bc,-ramStart
	add hl,bc
	sbc hl,bc
	; Ensure top 16 bits of BC are 0
	inc.s bc
	jr c,_
	ld c,9
	add hl,bc
	ld c,(hl)
	add hl,bc
_
	ld c,1+11
	add hl,bc
	ret
	
	; Return C if (DE) < (HL)
	; Case-insensitive by virtue of ignoring bit 5
	; Destroys: AF,BC,DE,HL,AF'
CompareDescriptions:
	; Get the minimum length of the two descriptions
	ld b,(hl)
	ld a,(de)
	cp b
	jr nc,_
	ld b,a
_
	; Save the length comparison result in shadow carry flag
	ex af,af'
	ld c,$DF
	inc b
_
	dec b
	jr z,_
	inc hl
	inc de
	; Check if the two bytes match, ignoring bit 5
	ld a,(de)
	xor (hl)
	and c
	jr z,-_
	; Copy bit 5 of (HL) into A and lexicographically compare
	xor (hl)
	cp (hl)
	ret
_
	; If the strings match entirely, return the length comparison
	ex af,af'
	ret
	
; Compares the buffers at HL and DE, with size BC. Returns Z if equal.
; HL and DE point after the first non-matching characters
; (or the end if all characters match)
memcmp:
	ld a,(de)
	inc de
	cpi
	ret po
	jr z,memcmp
	ret
	
; Adds up BC bytes at HL. Output in IX.
checksum:
	ld ix,0
	lea de,ix
_
	ld e,(hl)
	add ix,de
	cpi
	ret po
	jr -_
	
	; Returns BC=0, DE=byte|byte|byte
#macro MEMSET_FAST(start, length, byte)
	ld hl,start + length
	ld de,byte * $010101
	ld bc,(((length / 24) + 1) % 256 * 256) | (((length / 24) / 256) + 1)
	ld a,((8 - (length / 3 % 8)) * 4) | ((3 - (length % 3)) % 2 * 2) | ((3 - (length % 3)) / 2 % 2)
	call memset_fast
#endmacro

	; Also sets SPS to the end of the buffer
#macro MEMSET_FAST_SPS(start, length, byte)
	ld hl,start + length
	ld.s sp,hl
	ld de,byte * $010101
	ld bc,(((length / 24) + 1) % 256 * 256) | (((length / 24) / 256) + 1)
	ld a,((8 - (length / 3 % 8)) * 4) | ((3 - (length % 3)) % 2 * 2) | ((3 - (length % 3)) / 2 % 2)
	call memset_fast
#endmacro
	
memset_fast:
	ld (memset_fast_save_sp),sp
	ld sp,hl
	or a
	sbc hl,hl
	rra
	adc hl,hl
	rra
	ld (memset_fast_smc),a
	adc hl,hl
memset_fast_smc = $+1
	jr $+2
memset_fast_loop:
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	djnz memset_fast_loop
	dec c
	jr nz,memset_fast_loop
	add hl,sp
	ld sp,hl
	push de
memset_fast_save_sp = $+1
	ld sp,0
	ret
	
; Gets a pointer to the current menu in HL, the current selection index in BC,
; and sets the Z flag if the Load ROM menu is active.
get_current_menu_selection:
	ld hl,MenuList
current_menu = $+1
	ld c,0
	ld b,2
	mlt bc
	add hl,bc
	; Check if menu #1 (Load ROM)
	dec c
	dec c
	ld bc,(ArcBase)
	add hl,bc
	ld hl,(hl)
	dec.s hl
	add hl,bc
current_menu_selection = $+1
	ld bc,main_menu_selection
	ret
	
; Resolves the config item at IX via game and global config,
; and advances to the next item.
; Returns Z flag set if item was inherited from global config.
read_config_item:
	ld a,(ix-config_start+game_config_start)
	inc ix
	cp $FF
	ret nz
	ld a,(ix-1)
	ret
	
; Checks whether the digit item is load state and the state exists
check_valid_load_state:
	; Return zero if no ROM loaded
	or a
	ret z
	; Return non-zero if not Load State
	inc hl
	ld a,(hl)
	dec hl
	or a
	ret nz
check_valid_state:
	ld a,(current_state)
	sub '0'-1
	ld bc,(existing_state_map)
_
	srl b
	rr c
	dec a
	jr nz,-_
	adc a,a
	ret
	
; Sets the current string BG color. Must be called before SetStringColor.
SetStringBgColor:
	ld (PutChar_BgColorSMC1),a
	ld (PutChar_BgColorSMC2),a
	ret
	
; Sets the current string color.
SetStringColor:
PutChar_BgColorSMC1 = $+1
	xor BLUE
	ld (PutChar_ColorSMC),a
	ret

; Renders the character in A on the current buffer at (*B, *C) and updates the
; cursor location.
PutCharXY:
	ld (cursorRowCol),bc

; Renders the character in A on the current buffer at (*cursorCol, *cursorRow).
PutChar:
	ld bc,0
cursorRowCol = $-3
; The current text output row, in pixels (0-230).
cursorRow = cursorRowCol
; The current text output column, in characters (0-39).
cursorCol = cursorRow+1
	; Convert to font index. 0 is treated as a space, and 1-31 as newline.
	or a
	jr z,_
	sub ' '
	jr nc,_
	AJUMP(PutNewLine)
_
	; Put pointer to character data in appvar in DE.
	ld hl,(ArcBase)
	ld de,font
	add hl,de
	ld e,a
	ld d,10
	mlt de
	add hl,de
	ex de,hl
	; Get pointer to buffer output location in HL.
	; Refuse to draw past the right side of the screen.
	ld a,b
	cp 40
	ret nc
	ld b,160
	mlt bc
	ld hl,(current_buffer)
	add hl,bc
PutChar_SmallBufferSMC1 = $
	add hl,bc
	ld c,a
	ld b,8
	mlt bc
	add hl,bc
	
	; Draw 10 rows of pixels.
	ld b,10
PutCharRowLoop:
	push bc
	 ; Get the bitmap for the current row in C.
	 ld a,(de)
	 inc de
	 cpl
	 ld c,a
	 ld b,8
PutCharPixelLoop:
	 ; Render 2 pixels to the framebuffer per iteration.
	 sla c
	 sbc a,a
PutChar_ColorSMC = $+1
	 and WHITE ^ BLUE
PutChar_BgColorSMC2 = $+1
	 xor BLUE
	 ld (hl),a
	 inc hl
	 djnz PutCharPixelLoop
	 ; Advance output pointer to the next row.
PutChar_SmallBufferSMC2 = $+1
	 ld c,(320-8)/2
	 add hl,bc
	 add hl,bc
	pop bc
	djnz PutCharRowLoop
	; Increment the cursor column.
	ld hl,cursorCol
	inc (hl)
	ret
	
DivHLIXBy60:
	ld c,60
DivHLIXByC:
	ld b,48
	xor a
_
	add ix,ix \ adc hl,hl \ rla
	cp c
	jr c,_
	sub c
	inc ix
_
	djnz --_
	ret
	
MulHLIXBy24:
	add ix,ix \ adc hl,hl
	add ix,ix \ adc hl,hl
	lea bc,ix
	push hl \ pop de
	add ix,ix \ adc hl,hl
	add ix,bc \ adc hl,de
	lea bc,ix
	push hl \ pop de
	add ix,ix \ adc hl,hl
	ret
	
	; Input: DE=byte stream to transfer, B=byte count
	; Output: DE follows byte stream, B=0
	; Destroys: AF, HL
spiFastTransfer:
#ifdef CEMU
	push bc
	 ; Start SPI transfer
	 ld c,1
	 ld hl,mpSpiTransfer
	 ld (hl),c
	 ex de,hl
	 inc b
_
	 dec b
	 jr z,+++_
	 ; Always read 2 bytes when byte-aligned
	 ld e,(hl) \ inc hl
	 dec b
	 jr z,$
_
	 ld a,e
	 ; Read next byte and shift into position
	 ld e,(hl) \ inc hl
	 ld d,c
	 mlt de
	 ; Combine with last byte
	 or d
	 ; Shift up low byte and send top 9 bits
	 sla e \ rla
	 ld d,3
_
	 rla \ rla \ rla
	 ld (mpSpiFifo),a
	 dec d
	 jr nz,-_
	 ; Update shift amount, and read two bytes if byte-aligned
	 rlc c
	 jr c,---_
	 ; Check for end of byte stream
	 djnz --_
_
	 ex de,hl
	pop hl
	ld c,l
	ld hl,mpSpiStatus
#else
	; Start SPI transfer
	ld hl,mpSpiTransfer
	ld (hl),1
	; Fill SPI FIFO and transfer at the same time
	ld l,mpSpiFifo & $FF
_
	ld a,(de)
	ld (hl),a
	inc de
	djnz -_
	; Wait for transfer to complete
	ld l,mpSpiStatus & $FF
#endif
_
	bit 2,(hl)
	jr nz,-_
	; Disable transfer, discarding excess bits sent to LCD
	ld l,mpSpiTransfer & $FF
	ld (hl),b
	ret
	
; The calculator type, 0=84+CE, 1=83PCE
calcType:
	.db 0
; The total number of ROMs.
romTotalCount:
	.db 0
; The first ROM in the current list frame.
romListFrameStart:
	.db 0
; The number of ROMs in the current list frame, plus 1.
romListFrameCount:
	.db 0
; The item corresponding to the currently selected ROM.
romListItem:
	.db ITEM_ROM,0
; The argument to pass to sprintf for error messages
errorArg:
	.dl 0
; A list of the number of days before each month
monthLUT:
	.db 0,31,28,31,30,31,30,31,31,30,31,30
; The number of days from 1/1/1970 to the RTC epoch.
epochDayCount:
	.dl 0
; The number of seconds offset for the selected time zone.
timeZoneOffset:
	.dl 0
	
; The name of the currently loaded ROM.
ROMName:
	.db appVarObj
	.block 9
; The name of the ROM to be loaded next.
ROMNameToLoad:
	.db appVarObj
	.block 9
; The ROM appvar magic header.
MetaHeader:
	.db "TIBOYCE",0
; The name of the appvar itself.
SelfName:
	.db appVarObj,"TIBoyDat"
; The name of the config file.
ConfigFileName:
	.db appVarObj,"TIBoyCfg"
; The name of the skin file.
SkinFileName:
	.db appVarObj,"TIBoySkn"
	
; The backup of SP before beginning emulation.
saveSP:
	.dl 0
; The pointer to the skin file data (or 0 if it doesn't exist)
skin_file_ptr:
	.dl 0
; The size of Game Boy cartridge RAM (plus 48 for RTC carts)
cram_size:
	.dl 0
; The start address of Game Boy cartridge RAM.
cram_start:
	.dl 0
; The cartridge's Memory Bank Controller type.
mbc:
	.db 0
; The mask to apply to ROM banks, depending on the size of the ROM.
rom_bank_mask:
	.db 0
	
; The number of frames left to skip, plus 1.
skippable_frames:
	.db 1
; The current buffer being displayed (front buffer).
current_display:
	.dl gb_frame_buffer_1
; The current buffer to render to (back buffer).
current_buffer:
	.dl gb_frame_buffer_2
; The default palette.
default_palette:
	.db 0
; The digits for performance display.
perf_digits:
	.db 0,0,0,0,10

; The output buffer for printf, large enough for two rows of text.
text_buffer:
	.block 83
	
; The duration of the displayed emulator message, in frames.
emulatorMessageDuration:
	.db 0
; The current emulator message text, null-terminated.
emulatorMessageText:
	.block 21
	
; The pointer to the currently selected menu item.
current_item_ptr:
	.dl 0
; The pointer to the active ROM description.
current_description = asm_data_ptr1
; A backup of the selected main menu option.
main_menu_selection:
	.db 1
; As well as the rest of the menus
	.db 2,1,1,1
; Index of the previous menu item
menuPrevItem:
	.db 0
; Index of the next menu item
menuNextItem:
	.db 0
; The currently chosen configuration to edit. Must precede current_state.
; 0=Global, 1=Game
current_config:
	.db 0
; The currently chosen save state index.
current_state:
	.db 0
; Boolean for whether to auto save state. Must follow current_state.
should_auto_save:
	.db 0
; A bitmap of existing save states for the currently loaded game.
existing_state_map:
	.dw 0
	
originalHardwareSettings:
	; IntEnable
	.dl 0
	; IntLatch
	.dl 0
	; FlashWaitStates
	.db 0
	; MBASE
	.db 0
	;mpKeypadScanMode
	.db 0
	;mpLcdImsc
	.db 0
	;mpRtcCtrl
	.db 0
	;mpSpiDivider
	.dl 0
	
originalLcdSettings:
	; LcdTiming0
	.block 12
	; LcdCtrl
	.dl 0
	; SPI settings
	.dw spiSetupDefault+1
	
; These files are loaded into RAM.
	#include "jit.asm"
	#include "decode.asm"
	#include "ophandler.asm"
	#include "vblank.asm"
	#include "waitloop.asm"
	#include "lzf.asm"
	
	.echo "User RAM code size: ", $ - userMem
	
	; Pad to an odd number of bytes
	.block (~$) & 1
	
; Active configuration info:
config_start:
	.dw config_end - ConfigVersion
	
ConfigVersion:
	.db 1
	
; Number of option bytes
	.db option_config_count
; The currently chosen frameskip value.
FrameskipValue:
	.db 2
	
OptionConfig:
; Frameskip type (0=Manual, 1=Auto, 2=Off)
FrameskipType:
	.db 1
; Speed display (0=Never, 1=Turbo, 2=Slowdown, 3=Always)
SpeedDisplay:
	.db 1
; Auto archive (0=Off, 1=On)
AutoSaveState:
	.db 1
; Palette selection (0=Default, 1...=Manual)
PaletteSelection:
	.db 0
; Time zone (0-31)
TimeZone:
	.db 0
; Daylight saving time (0=Off, 1=On)
DaylightSavingTime:
	.db 0
; Scaling mode (0=No scaling, 1=Fullscreen)
ScalingMode:
	.db 1
; Skin display (0=Off, 1=On)
SkinDisplay:
	.db 1
; Turbo mode (0=Toggle, 1=Hold)
TurboMode:
	.db 0
; Scaling type (0=static, 1=scrolling)
ScalingType:
	.db 1
; Message display (0=Off, 1=On)
MessageDisplay:
	.db 1
; Adjust colors (0=Off, 1=On)
AdjustColors:
	.db 1
; Confirm state save/load (Bit 0=Load, Bit 1=Save)
ConfirmStateOperation:
	.db 3
	
; Number of key bytes
	.db key_config_count
; Key configuration. Each is a GetCSC scan code.
KeyConfig:
	.db 51,3,2,4,1,54,48,40,55,15,42,43,44,10,11
	
config_end:
option_config_count = (KeyConfig-1) - FrameskipValue
key_config_count = config_end - KeyConfig
	
; The RAM program ends here. Should be at an odd address.
program_end:
program_size = program_end - userMem
	.echo "User RAM program size: ", program_size

; Space for the game-specific config is located at the end of the program.
game_config_start:
GameFrameskipValue = game_config_start + (FrameskipValue - config_start)
GameSkinDisplay = game_config_start + (SkinDisplay - config_start)
GameMessageDisplay = game_config_start + (MessageDisplay - config_start)
GameOptionConfig = game_config_start + (OptionConfig - config_start)
GameKeyConfig = game_config_start + (KeyConfig - config_start)
game_config_end = game_config_start + (config_end - config_start)

; The size of the save state is located after the game-specific config.
save_state_size_bytes = game_config_end

save_state_start = save_state_size_bytes + 3
; A saved copy of OAM/HRAM, when saving/loading state. 512 bytes in size.
hram_saved = save_state_start

; The start of saved registers, etc. Between the saved OAM and HRAM. 96 bytes in size.
regs_saved = hram_saved + $00A0

; The checksum of cart RAM
cart_ram_checksum = regs_saved + state_size - 3

; Start of Game Boy VRAM. 8KB in size. Must be 2-byte aligned.
vram_start = hram_saved + $0200
; Start of Game Boy WRAM. 8KB in size.
wram_start = vram_start + $2000

; Base address of VRAM, can be indexed directly by Game Boy address.
vram_base = vram_start - $8000
; Base address of WRAM, can be indexed directly by Game Boy address.
wram_base = wram_start - $C000

; The size of the inserted cartridge RAM is located at the end of the main save state.
save_state_end = wram_start + $2000
save_state_size = save_state_end - save_state_start

arc_offset = $15
	.org arc_offset+header_size+program_size
arc_start:
; These files remain in the archived appvar.
	#include "setup.asm"
	#include "text.asm"
	#include "menu.asm"
	#include "z80mode.asm"
	#include "render.asm"
arc_end:
arc_size = arc_end - arc_start

	.echo "Total size: ", arc_end-arc_offset
