#ifdef FASTLOG
#define FASTLOG_EVENT_RUNTIME_ERROR $DE
#define FASTLOG_EVENT_INVALID_OPCODE $AD
#define FASTLOG_EVENT_JIT_FLUSH $BE
#define FASTLOG_EVENT_CACHE_FLUSH $EF
#define FASTLOG_EVENT_RECOMPILE 1
#define FASTLOG_EVENT_RERECOMPILE 2
#define FASTLOG_EVENT_CACHE_MISS 3
#define FASTLOG_EVENT_PADDING_UPDATE 4
#define FASTLOG_EVENT_LOOKUP_GB 5
#define FASTLOG_EVENT_LOOKUP_GB_FOUND 6
#define FASTLOG_EVENT_LOOKUP_JIT 7
#define FASTLOG_EVENT_LOOKUP_JIT_INTERNAL 8
#define FASTLOG_EVENT_WAITLOOP_CHECK 9
#define FASTLOG_EVENT_WAITLOOP_IDENTIFIED 10
#define FASTLOG_EVENT_TRIGGER_EVENT 11

#macro FASTLOG_EVENT(type, length)
	#define EVENT_TYPE concat("FASTLOG_EVENT_", type)
	ld hl,(EVENT_TYPE << 16) | ((length+1) << 8)
	#undef EVENT_TYPE
	push hl
	call fastlog
	ld sp,hl
#endmacro

#macro FASTLOG_EVENT_Z80(type, length)
	#define EVENT_TYPE concat("FASTLOG_EVENT_", type)
	ld hl,(EVENT_TYPE << 8) | (length+1)
	#undef EVENT_TYPE
	push hl
	call fastlog_z80
	ld sp,hl
#endmacro
#endif

#define CALL_STACK_DEPTH 32
#define CALL_STACK_ENTRY_SIZE_Z80 4
#define CALL_STACK_ENTRY_SIZE_ADL 3

#define ERROR_CATCHER (Z80Error << 8) | $C3

#macro FIXME
	jr $
#endmacro

; Bypass a SPASM bug where forward-referenced values are not allowed in .fill
#macro SAFE_FILL(count, value)
	#if count > 0
	.db value
	SAFE_FILL(count - 1, value)
	#endif
#endmacro

#define .safe_fill SAFE_FILL(

#macro CPU_SPEED_START()
	#define CPU_SPEED_BASE eval($)
#endmacro

#macro CPU_SPEED_IMM(addr, width)
	#define CPU_SPEED_OFFSET eval(addr - CPU_SPEED_BASE)
	buf(1)
	#if CPU_SPEED_OFFSET < $40
	#define CPU_SPEED_VALUE eval((CPU_SPEED_OFFSET << 2) | width)
	wr(".db ", CPU_SPEED_VALUE)
	#else
	#define CPU_SPEED_VALUE eval((CPU_SPEED_OFFSET >> 8 << 2) | width | 1)
	wr(".db ", CPU_SPEED_VALUE)
	#define CPU_SPEED_VALUE eval(CPU_SPEED_OFFSET & $FF)
	wr(".db ", CPU_SPEED_VALUE)
	#endif
	#undef CPU_SPEED_VALUE
	#undef CPU_SPEED_OFFSET
#endmacro

#macro CPU_SPEED_IMM8(addr)
	CPU_SPEED_IMM(addr, 0)
	#define CPU_SPEED_BASE eval(addr)
#endmacro

#macro CPU_SPEED_IMM16(addr)
	CPU_SPEED_IMM(addr, 2)
	#define CPU_SPEED_BASE eval(addr+1)
#endmacro

#macro CPU_SPEED_END()
	buf(1)
	wr(".db 0")
	#undef CPU_SPEED_BASE
#endmacro

; Gets the 24-bit base pointer for a given Game Boy address.
; The base plus the address can be used to directly read GB memory.
;
; Inputs:  DE = GB address
; Outputs: HL = base pointer
; Destroys: F
#macro GET_BASE_ADDR_NO_ASSERT
	ld hl,z80codebase+mem_read_lut
	ld l,d
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	inc l \ inc l
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

#macro SPI_PARAM_CHECKED(param, limit)
	#if (param < 0) || (param > limit)
	.error "Parameter ", param, " not within limit ", limit
	#endif
	SPI_PARAM(param)
#endmacro

#macro SPI_PARAM2_CHECKED(param1, limit1, param2, limit2)
	#if (param1 < 0) || (param1 > limit1)
	.error "Parameter ", param1, " not within limit ", limit1
	#endif
	#if (param2 < 0) || (param2 > limit2)
	.error "Parameter ", param2, " not within limit ", limit2
	#endif
	SPI_PARAM((param2 << 4) | param1)
#endmacro

#macro SPI_END
	#if SPI_BIT != $80
	.db SPI_VALUE
	#endif
	#undef SPI_VALUE
	#undef SPI_BIT
#endmacro

; Gamma voltage levels are specified here.
; V0-V2, V20, V43, V61-V63 are the main points, ranging from 129 to 0.
; V4, V6, V13 are interpolated between V2 and V20, ranging from 60 to 0.
; V27, V36 are interpolated between V20 and V43, ranging from 25 to 0.
; V50, V57, V59 are interpolated between V43 and V61, ranging from 60 to 0.
; J0 and J1 are values between 0 and 3 affecting interpolations for remaining voltages.
#macro SPI_GAMMA(V0, V1, V2, V20, V43, V61, V62, V63, V4, V6, V13, V27, V36, V50, V57, V59)
	SPI_PARAM2_CHECKED(129-V0, $0F, 23-V63, $0F)
	SPI_PARAM_CHECKED(128-V1, $3F)
	SPI_PARAM_CHECKED(128-V2, $3F)
	SPI_PARAM_CHECKED(57-V4, $1F)
	SPI_PARAM_CHECKED(47-V6, $1F)
	SPI_PARAM2_CHECKED(21-V13, $0F, J0, $03)
	SPI_PARAM_CHECKED(128-V20, $7F)
	SPI_PARAM2_CHECKED(20-V27, $07, 11-V36, $07)
	SPI_PARAM_CHECKED(128-V43, $7F)
	SPI_PARAM2_CHECKED(54-V50, $0F, J1, $03)
	SPI_PARAM_CHECKED(44-V57, $1F)
	SPI_PARAM_CHECKED(34-V59, $1F)
	SPI_PARAM_CHECKED(64-V61, $3F)
	SPI_PARAM_CHECKED(64-V62, $3F)
#endmacro

; Sets both positive and negative gamma curves using the same parameters.
#macro SPI_GAMMA_BOTH(V0, V1, V2, V20, V43, V61, V62, V63, V4, V6, V13, V27, V36, V50, V57, V59)
	SPI_CMD($E0)
	SPI_GAMMA(V0, V1, V2, V20, V43, V61, V62, V63, V4, V6, V13, V27, V36, V50, V57, V59)
	SPI_CMD($E1)
	SPI_GAMMA(V0, V1, V2, V20, V43, V61, V62, V63, V4, V6, V13, V27, V36, V50, V57, V59)
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

; Palette entries representing remapped BGP colors.
BG_PALETTE_COLOR_0 = 255
BG_PALETTE_COLOR_1 = 0
BG_PALETTE_COLOR_2 = 1
BG_PALETTE_COLOR_3 = 2
; Constant color palette entries
BLUE = 9
MAGENTA = 10
OLIVE = 11
GRAY = 12
BLACK = 13
WHITE = 14
; Palette entries representing the raw BGP colors. Must precede OBP0 colors.
BG_COLOR_0 = 15
BG_COLOR_1 = 16
BG_COLOR_2 = 17
BG_COLOR_3 = 18
; Palette entries representing every possible combination of three OBJ colors.
; The data is overlapped such that a unique sequence of three colors begins
; at each entry. Note that this means each table is (64 + 2) entries large.
; Additionally, the original sequence of colors 0, 1, 2, 3 is present starting
; at the offset corresponding to palette %100100, which is $0D.
OBP0_COLORS_START = 19
OBP0_ORIG_COLORS = OBP0_COLORS_START+$0D
OBP1_COLORS_START = OBP0_COLORS_START + (64 + 2)
OBP1_ORIG_COLORS = OBP1_COLORS_START+$0D

; Palette entry representing transparent OBJ pixels on GBC.
; Never rendered, only used to determine priority against rendered pixels.
GBC_OBJ_TRANSPARENT_COLOR = 0
; Palette entries representing transparent BG pixels on GBC.
; One per palette, for 8 in total.
GBC_BG_TRANSPARENT_COLORS = 1
; Palette entries representing low-priority opaque OBJ pixels on GBC.
; Never rendered, only used to determine priority against rendered pixels.
; Three are allocated, to enable translation to actual OBJ colors.
GBC_OBJ_LOW_PRIO_COLORS = 12
; Palette entries representing normal-priority opaque BG pixels on GBC.
; Three per palette, for 24 in total.
GBC_BG_OPAQUE_COLORS = GBC_OBJ_LOW_PRIO_COLORS + 3
; Palette entries representing normal-priority opaque OBJ pixels on GBC.
; Never rendered, only used to determine priority against rendered pixels.
; Three are allocated, to enable translation to actual OBJ colors.
GBC_OBJ_NORMAL_PRIO_COLORS = GBC_BG_OPAQUE_COLORS + 24
; Palette entries representing high-priority opaque BG pixels on GBC.
; These represent the same literal colors as the normal-priority pixels.
; Three per palette, for 24 in total.
GBC_BG_HIGH_PRIO_COLORS = GBC_OBJ_NORMAL_PRIO_COLORS + 3
; Palette entries representing high-priority opaque OBJ pixels on GBC.
; Never rendered, only used to determine priority against rendered pixels.
; Three are allocated, to enable translation to actual OBJ colors.
GBC_OBJ_HIGH_PRIO_COLORS = GBC_BG_HIGH_PRIO_COLORS + 24
; Palette entries representing rendered opaque OBJ pixels on GBC.
; Three per palette, for 24 in total.
GBC_OBJ_OPAQUE_COLORS = GBC_OBJ_HIGH_PRIO_COLORS + 3

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
FPS = $D0258D
OPS = $D02593
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
VBLANK_SCANLINE = 144
SCANLINES_PER_VBLANK = SCANLINES_PER_FRAME - VBLANK_SCANLINE
CYCLES_PER_VBLANK = CYCLES_PER_SCANLINE * SCANLINES_PER_VBLANK

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

KEY1 = $ff4d
VBK = $ff4f
HDMA1 = $ff51
HDMA2 = $ff52
HDMA3 = $ff53
HDMA4 = $ff54
HDMA5 = $ff55
RP = $ff56
BGPI = $ff68
BGPD = $ff69
OBPI = $ff6a
OBPD = $ff6b
OPRI = $ff6c
SVBK = $ff70

IE = $ffff

; Memory areas used by the emulator

; The 16-bit Z80 address space starts here.
z80codebase = vRam
#ifdef SHADOW_STACK
; The Z80-mode shadow stack. A sliding window of the game's "main" stack.
; The "main" stack is set when a call/return is executed on a read/write stack.
; The memory LUTs are updated to direct other reads/writes into this window.
; When the stack pointer moves out of the window, it is shifted by 256 bytes.
shadow_stack_end = $FE00
shadow_stack_start = shadow_stack_end - 512
; The bottom of the Z80 stack. Grows down from the shadow stack start.
myz80stack = shadow_stack_start
#else
; The bottom of the Z80 stack. Grows down from the Game Boy HRAM start.
myz80stack = $FE00
#endif
; The lower bound of the call stack.
call_stack_lower_bound = myz80stack - 4 - (CALL_STACK_DEPTH * CALL_STACK_ENTRY_SIZE_Z80)
; The flags translation LUT.
flags_lut = myz80stack - 512
; The end of the trampoline high allocation pool.
trampoline_end = flags_lut - 3

; The bottom of the ADL stack. Grows down from the end of SafeRAM.
myADLstack = usbArea - 3

; Preprocessed Game Boy tilemap entries. 16KB in size.
; Game Boy only:
;   Each tile entry is a 2-byte offset of the pixel data from vram_pixels_start.
;   Every row of 32 tiles is duplicated into 64, to facilitate wraparound.
; Game Boy Color only:
;   Each tile entry is a 2-byte offset of the pixel data from vram_pixels_start,
;   followed by a 2-byte offset to the palette table from vram_pixels_start.
;   Bank/hflip/vflip attributes are included in the pixel data offset,
;   and palette/priority attributes are included in the palette table offset.
;   See gbc_tile_attributes_lut for a description of attribute bit mapping.
; In addition, each row is stored twice with different tilesets.
; So, each GB tilemap row takes a total of 256 bytes here.
; Buffer must be 256-byte aligned and contained within one 64KB-aligned block.
vram_tiles_start = (pixelShadow | $FF) + 1
decompress_buffer = vram_tiles_start

; LUT for LSB of green component adjustment.
; Input: Bit 7: Bit 2 of G
;        Bits 6-4: Bits 5-3 of B
;        Bits 3-2: Bits 1-0 of G
;        Bits 1-0: Bits 5-4 of G
; Must be 256-byte aligned and precede adjust_color_lut.
adjust_green_lsb_lut = vram_tiles_start + $4000

; LUT to remap the color's low byte for color adjustment.
; Input: Bits 5-7: Bits 2-0 of G
; Output: Bit 7: Bit 2 of G
;         Bits 3-2: Bits 1-0 of G
; Must be 256-byte aligned, and middle byte must equal $73 for optimization.
adjust_color_lut = adjust_green_lsb_lut + 256

; LUT for MSB of green component adjustment.
; Same input format as the LSB LUT.
; Must be 256-byte aligned and follow adjust_color_lut.
adjust_green_msb_lut = adjust_color_lut + 256

; Preprocessed Game Boy tile pixel entries. 24KB in size.
; Game Boy only:
;   Each tile is converted into one byte per pixel, for 64 bytes per tile.
; Game Boy Color only:
;   Each tile is converted into four bytes per row, for 32 bytes per tile.
;   Byte 0: Index into any palette table for the first 4 pixels.
;   Byte 1: Offset from the first 4 pixels to the last 4 pixels.
;   Byte 2: Index into any palette table for the last 4 pixels, reversed.
;   Byte 3: Offset from the last 4 pixels to the first 4 pixels, reversed.
;   Additionally, each VRAM bank is interleaved after each row,
;   which makes it simpler to represent the bank in the tile attributes.
;   This also effectively keeps tile lookups at a scale of 64.
; Buffer must be 256-byte aligned.
vram_pixels_start = adjust_green_msb_lut + 256
; The mini frame backup in the menu is temporarily stored in this area.
; 160 * 144 = 22.5 KB in size.
mini_frame_backup = vram_pixels_start

; Start of recompiler struct index lookup table. 512 bytes in size.
; The first 256 bytes are the LSBs and the next 256 are the MSBs.
; The lookup table is indexed by the high byte of a JIT address,
; and each pointer gives the last struct entry touching that range.
recompile_index_LUT = vram_pixels_start + $6000

; Start of recompiler cached jump index lookup table. 512 bytes in size.
; The first 256 bytes are the LSBs and the next 256 are the MSBs.
; The lookup table is indexed by the low byte of a GB address,
; and each pointer gives the first cache entry corresponding to that LSB.
recompile_cache_LUT = recompile_index_LUT + $0200

; A lookup table for converting BG palettes to raw colors. 256 bytes in size.
; Must be 256-byte aligned.
convert_palette_LUT = recompile_cache_LUT + $0200

; A table with the largest Y position of a sprite using the corresponding tile,
; or 0 if the tile is not used by any onscreen sprite.
; Must be 256-byte aligned.
oam_tile_usage_lut = convert_palette_LUT + 256

; Specifies offsets into a buffer of pixel data corresponding to the
; input 2bpp pixel data. Note that the input has the high palette bits
; grouped in the high nibble, and the low palette bits in the low nibble.
; Must be 256-byte aligned.
overlapped_pixel_index_lut = oam_tile_usage_lut + 256

; A table representing every possible combination of four 2bpp pixels.
; The data is overlapped such that a unique sequence of four pixels begins
; at each byte. Note that this means the table is 256 + 3 bytes large.
; Must be 256-byte aligned and directly follow the LUT. Game Boy only.
overlapped_pixel_data = overlapped_pixel_index_lut + 256

; Specifies offsets into a buffer of pixel data corresponding to the reverse
; of the input 2bpp pixel data. Note that the input has the high palette bits
; grouped in the high nibble, and the low palette bits in the low nibble.
; Must be 256-byte aligned and directly follow the non-reversed LUT. GBC only.
overlapped_pixel_rev_index_lut = overlapped_pixel_data

; Preconverted digit pixels for displaying FPS quickly. 24 bytes per character, 264 bytes total.
; Must be 8-byte aligned.
digits = (((overlapped_pixel_data + (256 + 3)) - 1) | 7) + 1

; Temporary backup for 16 palette entries. 32 bytes in size.
palette_backup = digits + 264

; Queue for BGP writes during a frame. 192+1 bytes in size.
; Stored in a compressed format, with either:
;   Byte 0 = Number of scanlines, Byte 1 = BGP value for all scanlines
;   Byte 0 = Number of scanlines (N) + 144, Bytes 1-N = BGP values per scanline
; Must end at a 256-byte aligned address. Game Boy only.
BGP_write_queue = (((palette_backup + 32) + 192) | 255) - 192

; Tracks the frequency of each BGP value as they are queued.
; The BGP value with the highest frequency will use the native palette.
; Must be 256-byte aligned and directly follow the queue. Game Boy only.
BGP_frequencies = BGP_write_queue + 192 + 1

; Converts GBC tile attributes for use in the low 6 bits of the tile cache.
; This bitmask is XORed with the tile row offset in bits 3-5 during rendering.
; Bit 1: Horizontal flip
; Bit 2: VRAM bank
; Bits 3-5: Vertical flip (all 1 or all 0)
; Must be 256-byte aligned. GBC only.
gbc_tile_attributes_lut = BGP_frequencies

; Specifies indices into an array of color data corresponding to the
; input 2bpp palette data. Note that the input is a BGP, OBP0, or OBJ1
; value, and identifies three colors corresponding to the upper 6 bits.
; Must be 256-byte aligned (and currently, follow BGP_frequencies).
overlapped_palette_index_lut = BGP_frequencies + 256

; Converts GBC tile attributes for use in 8x16 sprite rendering.
; This bitmask is XORed with the tile row offset in bits 3-6 during rendering.
; Bit 1: Horizontal flip
; Bit 2: VRAM bank
; Bits 3-6: Vertical flip (all 1 or all 0)
; Must be 256-byte aligned and follow the first LUT. GBC only.
gbc_tile_attributes_lut_2 = gbc_tile_attributes_lut + 256

; Table representing every possible combination of three BGP colors.
; The data is overlapped such that a unique sequence of three colors begins
; at each word. Note that this means the table is (64 + 2) * 2 bytes large.
; Additionally, the original sequence of colors 0, 1, 2, 3 is present starting
; at the index corresponding to palette %100100, which is $0D.
; This table must be 256-byte aligned. Game Boy only.
overlapped_bg_palette_colors = overlapped_palette_index_lut + 256
bg_palette_colors = overlapped_bg_palette_colors + ($0D*2)

; Table of GBC palette colors ready to copy to the native palette.
; There are 8 transparent BG colors, followed by 24 opaque BG colors,
; followed by 24 opaque OBJ colors, for a total of (8 + 24 + 24) * 2 bytes.
; This table must be 256-byte aligned. GBC only.
gbc_bg_transparent_colors = overlapped_bg_palette_colors
gbc_bg_opaque_colors = gbc_bg_transparent_colors + (8*2)
gbc_obj_opaque_colors = gbc_bg_opaque_colors + (24*2)

; Stack of cycle offset fixup locations during JIT block recompilation.
; Up to one pointer may be allocated for each recompiled opcode, meaning
; the stack size MAX_OPCODE_BYTES_PER_BLOCK * 2 = 124 bytes large.
; Conveniently this fits in a 256-byte space along with the BG palette colors.
recompile_cycle_offset_stack = overlapped_bg_palette_colors + 256

; List of LYC write predictions, based on writes in previous frames.
; The values in indices 1 to 144 correspond to the last value written to LYC
; after LYC matched LY at that index, only if the new value is between
; LY and 144. Otherwise, the value in the index equals 144.
; Index 0 is special, and indicates the first LYC value after vblank.
; This table must be 256-byte aligned.
lyc_prediction_list = recompile_cycle_offset_stack

; Two arrays of scanline information, one for each double buffer.
; Offset 0: Pointer to scanline sprite usage count.
; Offset 3: Pointer to the first pixel of the scanline in the frame buffer.
scanlineLUT_1 = lyc_prediction_list + 256
scanlineLUT_2 = scanlineLUT_1 + (144*6)

; One byte for each scanline indicating the number of sprites remaining
; in the current frame. Filled with 10 at the start of each frame, and
; decremented by 1 each time a sprite is rendered on that scanline.
scanline_sprite_counts = scanlineLUT_2 + (144*6)

; A list of VAT entries for found ROM files. 256 pointers in size.
romListStart = scanline_sprite_counts + 144

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Boy Color only data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tables mapping sprite attributes to palette offsets.
; The first table applies to high sprite priority,
; and the second table applies to low/normal sprite priority.
; Must be at a 256-byte offset from a 512-byte alignment.
high_prio_sprite_palette_lut = ((romListStart + (256*3) - 257) | $1FF) + 257
low_normal_prio_sprite_palette_lut = high_prio_sprite_palette_lut + 256

; Tables representing every possible combination of four sprite pixels.
;
; The pixels contained in these LUTs correspond to indices with no color,
; and rather are compared to pixels in the framebuffer to determine priority.
; Transparent pixels are lower priority than every other pixel, and are never
; drawn. Opaque pixels have values that can be offset to produce sprite pixels.
;
; The data is overlapped such that a unique sequence of four pixels begins
; at each byte. Sprite rendering uses wrapping logic rather than LDIR,
; so only 256 bytes are required per table.
; Must be 256-byte aligned, and follow the palette LUTs.
high_prio_sprite_pixel_lut = low_normal_prio_sprite_palette_lut + 256
normal_prio_sprite_pixel_lut = high_prio_sprite_pixel_lut + 256
low_prio_sprite_pixel_lut = normal_prio_sprite_pixel_lut + 256

; Tables representing every possible combination of four BG pixels, for each
; possible palette. High-priority BG tiles are considered as unique palettes,
; so there are 8*2=16 total palettes. Opaque pixels in high-priority tiles
; render with larger pixel values referring to duplicated colors.
; The data is overlapped such that a unique sequence of four pixels begins
; at each byte. Additionally, the data is repeated such that the copy pointer
; can move forward directly from one set of 4 pixels to the next 4 pixels.
; As such, each table is (256+3)*2 bytes in size, for 8288 bytes in total.
; The first LUT must be 512-byte aligned, to allow using INC H from the start
; of each palette to get a contiguous 256-byte region.
gbc_overlapped_pixel_data = low_prio_sprite_pixel_lut + 256
gbc_overlapped_pixel_data_end = gbc_overlapped_pixel_data + ((256+3)*2*16)

; The ROM bank lookup table. 256 entries in size.
; Offset 0: The MSB of the first trimmed portion of the bank, minus 1 ($3F-$7F).
; Offset 1: The base address of the ROM bank, minus $4000.
; The pre-subtracted $4000 means that the memory can be indexed by GB address.
; This must be located beyond the decompression buffer.
rombankLUT_end = (usbArea & ~$FF) - 256
rombankLUT = rombankLUT_end - (256*4)

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
	
fill_sprite_palette_luts:
	call fill_sprite_palette_lut
	ld a,GBC_OBJ_OPAQUE_COLORS - GBC_OBJ_NORMAL_PRIO_COLORS
	call fill_half_sprite_palette_lut
	ld a,GBC_OBJ_OPAQUE_COLORS - GBC_OBJ_LOW_PRIO_COLORS
fill_half_sprite_palette_lut:
	ld c,128-8
fill_sprite_palette_lut:
	ld b,8
	push hl
	pop de
_
	ld (de),a
	inc de
	add a,3
	djnz -_
	ldir
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
PutChar_SmallBufferSMC1 = $
	add hl,hl
	ld bc,(current_buffer)
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
	
spiFastTransferArc:
	ld hl,(ArcBase)
	add hl,de
spiFastTransferHL:
	ex de,hl
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
	
#ifdef FASTLOG
fastlog_z80_helper:
	or a
	sbc hl,hl
	add.s hl,sp
	push de
	 ld de,z80codebase+2
	 add hl,de
	pop de
	call _
	jp.sis z80_ret
	
fastlog:
	ld hl,4
	add hl,sp
_
	ld a,(hl)
	inc hl
	push bc
	 push de
fastlog_output_addr = $+1
	  ld de,0
fastlog_output_len = $+1
	  ld bc,0
fastlog_output_loop:
	  ldi
	  call po,fastlog_init
	  dec a
	  jr nz,fastlog_output_loop
fastlog_init_finish:
	  ld (fastlog_output_addr),de
	  ld (fastlog_output_len),bc
	 pop de
	pop bc
	ret
	
fastlog_init:
	push hl
	 ld hl,(OPS)
	 ld de,(FPS)
	 or a
	 sbc hl,de
	 ld (fastlog_output_len),hl
	 ex (sp),hl
	pop bc
	ld (fastlog_output_addr),de
	ret
	
fastlog_dump_to_save:
	ld hl,(fastlog_output_addr)
	ld de,(FPS)
	or a
	sbc hl,de
	ex de,hl
	ld hl,save_state_size_bytes
	ld bc,(hl)
	add hl,bc
	inc hl
	inc hl
	ld bc,(hl)
	add hl,bc
	inc hl
	ex de,hl
	dec bc
	jr z,++_
	sbc hl,bc
	jr c,_
	ld hl,(fastlog_output_addr)
	dec hl
	lddr
	ret
_
	push hl
	 add hl,bc
	 push hl
	 pop bc
	 ld hl,(fastlog_output_addr)
	 dec hl
	 lddr
	pop bc
	or a
	sbc hl,hl
	sbc hl,bc
	push hl
	pop bc
_
	ld hl,(OPS)
	dec hl
	lddr
	ret
#endif
	
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
; The start of the first bank of Game Boy cartridge ROM.
rom_start:
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
	
; The current text output column, in characters (0-39).
cursorCol:
	.db 0
; The current text output row, in pixels (0-230).
cursorRow:
	.db 0
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
	#include "trampoline.asm"
	#include "vblank.asm"
	#include "waitloop.asm"
	#include "lzf.asm"
	
	.echo "User RAM code size: ", $ - userMem
	
	; Pad to align vram_gbc_start
	.block (7-$) & 15
	
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
; Select preferred model (0=GB, 1=GBC)
PreferredModel:
	.db 1
	
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
GamePreferredModel = game_config_start + (PreferredModel - config_start)
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
; Start of Game Boy Color VRAM. 16KB in size. Must be 16-byte aligned.
vram_gbc_start = vram_start
#if vram_gbc_start & $0F
	.error "GBC VRAM is not 16-byte aligned!"
#endif

; Start of Game Boy WRAM. 8KB in size.
wram_start = vram_start + $2000
; Start of Game Boy Color WRAM. 32KB in size.
wram_gbc_start = vram_gbc_start + $4000

; Base address of VRAM, can be indexed directly by Game Boy address.
vram_base = vram_start - $8000
vram_gbc_base = vram_gbc_start - $8000
; Base address of WRAM, can be indexed directly by Game Boy address.
wram_base = wram_start - $C000
wram_gbc_base = wram_gbc_start - $C000

; BG palette memory, used by save state only. 64 bytes in size.
bg_palettes_saved = wram_gbc_start + $8000
; OBJ palette memory, used by save state only. 64 bytes in size.
obj_palettes_saved = bg_palettes_saved + $0040

; The size of the inserted cartridge RAM is located at the end of the main save state.
save_state_end = wram_start + $2000
save_state_gbc_end = obj_palettes_saved + $0040

save_state_size = save_state_end - save_state_start
save_state_gbc_size = save_state_gbc_end - save_state_start
	
; These files remain in the archived appvar.
	#include "setup.asm"
	#include "text.asm"
	#include "menu.asm"
	#include "z80mode.asm"
	#include "render.asm"
	#include "render_gbc.asm"

CpuSpeedRelocs:
	.dw vblank_counter
	.dw persistent_vblank_counter
	.dw ppu_counter
	.dw nextupdatecycle_STAT
	.dw nextupdatecycle_LY
	buf(1)
	run()

	.echo "Total size: ", program_size + $