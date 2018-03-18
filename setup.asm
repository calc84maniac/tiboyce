MENU_ITEM_COUNT = 15
	
	.org 0
Startup:
	ld (ArcBase),hl
	call _RunIndicOff
	ACALL(LoadConfigFile)
	
CalculateEpochLoop:
	; Grab current day count
	ld hl,(mpRtcDayCount)
	push hl
	
	 ; Multiply year by 365
	 ld hl,(osYear)
	 push hl
	 pop de
	 ld b,(365-1) / 2
_
	 add hl,de
	 add hl,de
	 djnz -_
	
	 ; Add in day-1
	 ld a,(osDay)
	 ld c,a
	 dec.s bc
	 add hl,bc
	
	 ; If we're Feb. or earlier, don't count this leap year
	 ld a,(osMonth)
	 cp 3
	 jr nc,_
	 dec de
_
	
	 ; Add in months
	 ld ix,monthLUT
_
	 ld c,(ix)
	 inc ix
	 add hl,bc
	 dec a
	 jr nz,-_
	
	 ; Add in year/4
	 srl d
	 rr e
	 srl d
	 rr e
	 add hl,de
	
	 ; Subtract out year/100
	 ex de,hl
	 ld a,25
	 call _DivHLByA
	 ex de,hl
	 or a
	 sbc hl,de
	
	 ; Add in year/400
	 srl d
	 rr e
	 srl d
	 rr e
	 add hl,de
	
	 ; Subtract out January 1, 1970
	 ld bc,-719527
	 add hl,bc
	 jr c,_
	 sbc hl,hl
_
	 ex de,hl
	
	pop bc
	; Make sure day count matches
	ld hl,(mpRtcDayCount)
	or a
	sbc hl,bc
	jr nz,CalculateEpochLoop
	ex de,hl
	sbc hl,bc
	ld (epochDayCount),hl
	
	or a
	sbc hl,hl
	ld (menuFrame),hl
	
RepopulateMenu:
	ld ix,rombankLUT
	push ix
	 ACALL(ROMSearch)
	pop bc
	lea hl,ix
	ld (ROMListEnd),hl
	or a
	sbc hl,bc
	ex de,hl
	
	ld hl,(menuFrame)
	sbc hl,bc
	jr c,RepopulateMenuTop
	sbc hl,de
	jr nc,RepopulateMenuTop
	
	ld hl,(menuSelection)
	or a
	sbc hl,bc
	jr c,RepopulateMenuTop
	sbc hl,de
	jr c,RedrawMenuClear
	
RepopulateMenuTop:
	ld (menuFrame),bc
	ld (menuSelection),bc
	
RedrawMenuClear:
	ACALL(RestoreHomeScreen)
	ld a,32
	ld (penRow),a
	ld hl,91
	ld (penCol),hl
	ld hl,$0031
	ld.sis (drawFGColor-ramStart),hl
	APTR(EmulatorTitle)
	call _VPutS
	ld a,227
	ld (penRow),a
	ld hl,18
	ld (penCol),hl
	APTR(WebsiteURL)
	call _VPutS
	or a
	sbc hl,hl
	ld.sis (drawFGColor-ramStart),hl
	ld c,l
RedrawMenu:
	ld de,(menuFrame)
	ld a,34
	ld b,MENU_ITEM_COUNT
RedrawMenuLoop:
	add a,12
	ld (penRow),a
	; Carry is clear
	sbc hl,hl
	ld (penCol),hl
	ld hl,(ROMListEnd)
	sbc hl,de
	jr z,RedrawMenuLoopEnd
	ld hl,(menuSelection)
	sbc hl,de
	jr z,RedrawMenuSelectedItem
	xor a
	ld hl,(menuLastSelection)
	sbc hl,de
	jr z,RedrawMenuItem
	or c
	jr z,RedrawMenuItem
	jr RedrawMenuSkipItem
NoROMsFound:
	APTR(ErrorNoROMsFound)
	call _VPutS
_
	halt
	call _GetCSC
	or a
	jr z,-_
	AJUMP(SaveConfigAndQuit)
RedrawMenuSelectedItem:
	set textInverse,(iy+textFlags)
RedrawMenuItem:
	push bc
	 push de
	  ex de,hl
	  ld hl,(hl)
	  ACALL(DrawMenuItem)
	 pop de
	pop bc
	res textInverse,(iy+textFlags)
RedrawMenuSkipItem:
	inc de
	inc de
	inc de
	ld a,(penRow)
	djnz RedrawMenuLoop
	
RedrawMenuLoopEnd:
	ld a,b
	cp MENU_ITEM_COUNT
	jr z,NoROMsFound
	ld hl,(menuSelection)
	ld (menuLastSelection),hl
	
SelectionLoop:
	halt
	call _GetCSC
	ld hl,(menuSelection)
	ld de,(ROMListEnd)
	ld bc,MENU_ITEM_COUNT*3
	sub 1
	jr z,MenuDown
	dec a
	jr z,MenuLeft
	dec a
	jr z,MenuRight
	dec a
	jr z,MenuUp
	cp 9-4
	jr z,MenuEnterTrampoline
	cp 54-4
	jr z,MenuEnterTrampoline
	cp 15-4
	jr nz,SelectionLoop
	jr SaveConfigAndQuit
	
MenuEnterTrampoline:
	AJUMP(MenuEnter)
	
MenuDown:
	inc hl
	inc hl
	inc hl
	sbc hl,de
	jr z,SelectionLoop
	add hl,de
	ld (menuSelection),hl
	ld a,(menuFrame)
	sub l
	add a,c
	jr z,RedrawMenuClearTrampoline
RedrawMenuTrampoline:
	AJUMP(RedrawMenu)
	
MenuLeft:
	ld de,rombankLUT
	sbc hl,de
	sbc hl,bc
	jr nc,_
	sbc hl,hl
_
	adc hl,de
	ld (menuSelection),hl
	jr c,RedrawMenuTrampoline
	ld hl,(menuFrame)
	sbc hl,bc
	jr RedrawMenuClearTrampoline
	
MenuUp:
	ld de,rombankLUT
	sbc hl,de
	jr z,SelectionLoop
	add hl,de
	ld a,(menuFrame)
	cp l
	dec hl
	dec hl
	dec hl
	ld (menuSelection),hl
	ld de,-(MENU_ITEM_COUNT-1)*3
	add hl,de
	jr nz,RedrawMenuTrampoline
RedrawMenuClearTrampoline:
	ld (menuFrame),hl
	AJUMP(RedrawMenuClear)
	
MenuRight:
	add hl,bc
	sbc hl,de
	jr c,_
	ld hl,-3
_
	add hl,de
	ld (menuSelection),hl
	ld hl,(menuFrame)
	add hl,bc
	sbc hl,de
	jr nc,RedrawMenuTrampoline
	add hl,de
	jr RedrawMenuClearTrampoline
	
SaveConfigAndQuit:
	ACALL_SAFERET(SaveConfigFile)
RestoreHomeScreen:
	ld hl,pixelShadow
	ld de,pixelShadow+1
	ld bc,(8400*3) - 1
	ld (hl),0
	ldir
	set graphDraw,(iy+graphFlags)
	call _DrawStatusBar
	call _HomeUp
	ei
	jp _ClrLCDFull
	
MenuEnter:
	ld hl,(hl)
	ld de,-6
	add hl,de
	ld b,(hl)
	ld de,ROMName+1
_
	dec hl
	ld a,(hl)
	ld (de),a
	inc de
	djnz -_
	xor a
	ld (de),a
	
	ACALL_SAFERET(StartROM)
	jr nc,RepopulateMenuTrampoline
	or a
	jr z,SaveConfigAndQuit
	push af
	 ld a,32
	 ld (penRow),a
	 sbc hl,hl
	 ld (penCol),hl
	 push hl
	  call _ClrLCDFull
	  APTR(error_text)
	  call _VPutS
	 pop bc
	pop de
	
	dec hl
	xor a
_
	cpir
	dec d
	jr nz,-_
	
	ex de,hl
	ld hl,(errorArg)
	push hl
	 push de
	  ld hl,text_buffer
	  push hl
	   call _sprintf
	  pop hl
	  call _VPutS
	 pop de
	pop hl
	
_
	halt
	call _GetCSC
	or a
	jr z,-_
	
RepopulateMenuTrampoline:
	AJUMP(RepopulateMenu)
	
StartROM:
	ACALL_SAFERET(LoadROMAndRAM)
	ret c
	
	ld a,'A' - '0'
	ld (current_state),a
	ACALL(LoadStateFile)
	ld a,0
	ld (current_state),a
	jr nc,StartFromHere
	
RestartFromHere:
	ld hl,hram_saved
	push hl
	pop de
	inc de
	ld bc,ram_size - hram_saved - 1
	ld (hl),0
	ldir
	
	APTR(regs_init)
	ld de,regs_saved
	ld bc,hmem_init - regs_init
	ldir
	
	ld de,hram_saved + $0100
	ld bc,$0100
	ldir
	
StartFromHere:
	ld hl,SkinFileName
	ACALL(LookUpAppvar)
	jr nc,_
	or a
	sbc hl,hl
_
	ld (skin_file_ptr),hl
	
	di
	push iy
	ld hl,mpFlashWaitStates
	push hl
	ld a,(hl)
	push af
	ld (hl),2
	ld hl,(mpIntEnable)
	push hl
	ld hl,(mpIntLatch)
	push hl
	ld hl,$000801
	ld (mpIntEnable),hl
	ld hl,$000011
	ld (mpIntLatch),hl
	
	ld hl,(mpLcdBase)
	push hl
	ld hl,(mpLcdCtrl)
	push hl
	ACALL(GetLCDTiming)
	
	APTR(palettecode)
	ld de,mpLcdPalette
	ld bc,palettecodesize
	ldir
	
	ld hl,$0C25
	ld (mpLcdCtrl),hl
	
	ld hl,scanlineLUT_1
	ld (scanlineLUT_ptr),hl
	
	ld hl,gb_frame_buffer_1
	ld (mpLcdBase),hl
	ld (current_buffer),hl
	push hl
	pop de
	inc de
#if 0
	ld a,BLACK_BYTE
	call SetStringBgColor
	ld bc,160*144
	ld (hl),WHITE_BYTE
	ldir
	ld bc,160*96
	ld (hl),a
	ldir
	ld bc,160*144
	ld (hl),WHITE_BYTE
	ldir
	ld bc,160*96-1
	ld (hl),a
	ldir
	ld hl,230*256
	ld (cursorCol),hl
	ld a,WHITE
	call SetStringColor
#else
	ld bc,320*240-1
	ld (hl),BLACK_BYTE
	ldir
#endif
	
#ifdef DEBUG
	APRINTF(StartText)
#endif
	
	ld a,3
	ld (mpKeypadScanMode),a
	inc a
	ld (mpLcdImsc),a
	
	ld (saveSP),sp
	
	ld sp,myADLstack
	
	ld hl,z80codebase
	push hl
	pop de
	inc de
	ld bc,hram_start - z80codebase - 1
	ld (hl),l
	ldir
	ld hl,hram_saved
	ld b,2
	ldir
	
	APTR(cursorcode)
	ld de,mpLcdCursorImg
	ld bc,cursorcodesize
	ldir
	
	APTR(z80code)
	ld de,z80codebase
	ld bc,z80codesize
	ldir
	
	APTR(flags_lut_init)
	ld de,z80codebase + flags_lut
	inc b
	ldir
	
	ld hl,convert_palette_LUT
_
	ld (hl),l
	inc l
	jr nz,-_
	
	ld hl,vram_start
	ld de,vram_pixels_start
	ld c,$0C
_
	push hl
	 ld hl,(hl)
	 push bc
	  ld b,8
_
	  sla h
	  ccf
	  sbc a,a
	  or $01	; The VRAM will be converted later to double scale if needed
	  sla l
	  jr nc,$+5 \ rlca \ adc a,0
	  ld (de),a
	  inc de
	  djnz -_
	 pop bc
	pop hl
	inc hl
	inc hl
	djnz --_
	dec c
	jr nz,--_
	
	ld a,vram_tiles_start >> 16
	ld mb,a
	ld ix,vram_tiles_start + 128
	ld c,$40
	ld a,c
_
	ld b,$20
_
	ld e,(hl)
	inc hl
	ld d,a
	mlt de
	ld.s (ix-128),de
	ld.s (ix-64),de
	bit 5,d
	jr nz,_
	set 6,d
_
	ld.s (ix),de
	ld.s (ix+64),de
	lea ix,ix+2
	djnz --_
	lea ix,ix-64
	inc ixh
	dec c
	jr nz,---_
	
	ld hl,fake_tile
	push hl
	pop de
	inc de
	ld (hl),$FF
	ld c,64
	ldir
	ld (hl),b
	ld a,(fake_tile - vram_pixels_start) >> 8
	ld (de),a
	inc de
	ld c,40
	ldir
	
	ACALL(IdentifyDefaultPalette)
	ACALL(ApplyConfiguration)
	
	ld a,z80codebase >> 16
	ld mb,a
	
	ld iy,state_start+state_size
	
	ld.sis sp,myz80stack
	ld hl,ophandlerRET
	push.s hl
	
	ld a,(iy-state_size+STATE_ROM_BANK)
	ld (z80codebase+curr_rom_bank),a
	ld (z80codebase+rom_bank_check_smc_1),a
	ld (z80codebase+rom_bank_check_smc_2),a
	ld c,a
	ld b,3
	mlt bc
	ld hl,rombankLUT
	add hl,bc
	ld hl,(hl)
	ld (rom_bank_base),hl
	
	ld a,(ram_size+1)
	and $80
	add a,$B7	; or a / scf
	ld (z80codebase+ram_size_smc),a
	
	xor a
	ld (exitReason),a
	ld (memroutine_rtc_smc_1),a
	ld a,$18	;JR
	ld (memroutine_rtc_smc_2),a
	
	ld hl,(cram_start)
	ld bc,$A000
	sbc hl,bc
	ld (z80codebase+cram_base_0),hl
	
	ld a,(mbc)
	ld (z80codebase+mbc_z80),a
	cp 4	;MBC3+RTC
	jr nz,setup_ram_bank
	
	; Update rtc_last
	call update_rtc
	
	ld ix,ram_size-46
	ld bc,(ix+46)
	add ix,bc
	ld b,10
	ld de,z80codebase+rtc_latched
_
	ld a,(ix)
	ld (de),a
	lea ix,ix+4
	inc de
	djnz -_
	
	; Ignore timestamps in the future
	push ix
	 ACALL(GetUnixTimeStamp)
	 ex (sp),ix
	 ex (sp),hl
	 ld de,(ix)
	 or a
	 sbc hl,de
	pop hl
	ld de,(ix+3)
	sbc hl,de
	jr c,_
	
	ld hl,(ix+6)
	ld a,h
	or l
	jr nz,_
	ex de,hl
	ld ix,(ix)
	ACALL(ExtractUnixTimeStamp)
	lea hl,ix
	ld de,(epochDayCount)
	or a
	sbc hl,de
	ld.sis (rtc_last),bc
	ld.sis (rtc_last+2),a
	ld.sis (rtc_last+3),hl
_
	ld a,(iy-state_size+STATE_RAM_BANK)
	sub 8
	jr c,setup_ram_bank
	ld c,a
	call mbc_rtc_toggle_smc
	call update_rtc
	ld b,0
	ld hl,z80codebase+rtc_latched
	jr _
	
setup_ram_bank:
	ld a,(ram_size+1)
	add a,a
	sbc a,a
	and (iy-state_size+STATE_RAM_BANK)
	and 3
	
	rrca
	rrca
	rrca
	ld b,a
	ld c,0
	ld hl,(z80codebase+cram_base_0)
_
	add hl,bc
	ld (cram_bank_base),hl
	
	ld a,(iy-state_size+STATE_MBC_MODE)
	and 1
	ld (mbc_rtc_last_latch),a
	jr z,_
	ld a,$20 ;JR NZ (overriding JR Z)
	ld (z80codebase+mbc1_ram_smc),a
_
	
	ld a,(iy-state_size+STATE_INTERRUPTS)
	ld (z80codebase+intstate),a
	
	ld de,(iy-state_size+STATE_DIV_COUNTER)
	ld.sis (div_cycle_count),de
	
	ld a,(iy-ioregs+TAC)
	bit 2,a
	jr z,_
	and 3
	ld c,a
	ld hl,timer_smc_data
	add hl,bc
	add hl,bc
	ld a,(hl)
	ld (z80codebase+updateTIMA_smc),a
	inc hl
	ld h,(hl)
	ld a,(iy-ioregs+TIMA)
	cpl
	ld l,a
	ld a,h
	ld (z80codebase+timer_cycles_reset_factor_smc),a
	ld (writeTIMA_smc),a
	mlt hl
	add hl,hl
	add hl,de
	add a,a
	dec a
	or l
	ld l,a
	inc hl
	ld.sis (timer_cycle_target),hl
_
	
	ld a,(iy-ioregs+SC)
	cpl
	and $81
	jr nz,_
	ld hl,(iy-state_size+STATE_SERIAL_COUNTER)
	ld.sis (serial_cycle_count),hl
	ld a,$38 ;JR C (overriding JR NC)
	ld (z80codebase+serial_enable_smc),a
_
	
	ld hl,(iy-ioregs+LCDC-2)
	add hl,hl
	jr c,_
	ld a,$20 ;JR NZ (overriding JR Z)
	ld (LCDC_7_smc),a
_
	add hl,hl
	add hl,hl
	jr nc,_
	ld a,$38 ;JR C (overriding JR NC)
	ld (LCDC_5_smc),a
_
	add hl,hl
	jr nc,_
	xor a ;(overriding $80)
	ld (LCDC_4_smc),a
	ld (window_tile_ptr),a
_	
	add hl,hl
	jr nc,_
	ld a,((vram_tiles_start + $2000) >> 8) & $FF
	ld (LCDC_3_smc),a
_
	add hl,hl
	jr nc,_
	ld a,$78 ;(overriding $38)
	ld (LCDC_2_smc_1),a
	ld a,15 ;(overriding 7)
	ld (LCDC_2_smc_2),a
	ld (LCDC_2_smc_4),a
	ld a,$81 ;RES 0,C (overriding RES 0,B)
	ld (LCDC_2_smc_3),a
	ld a,1 ;(overriding 9)
	ld (LCDC_2_smc_5),a
_
	add hl,hl
	add hl,hl
	jr c,_
	ld a,$31 ;LD SP (overriding ADD HL,SP)
	ld (LCDC_0_smc),a
_	
	
	ld hl,(iy-ioregs+SCY)
	ld a,l
	ld (SCY_smc),a
	ld a,h
	rrca
	rrca
	and $3E
	ld (SCX_smc_1),a
	ld a,h
	cpl
	and 7
	inc a
	ld (SCX_smc_2),a
	
	ld a,(iy-ioregs+LYC)
	or a
	ld hl,CYCLES_PER_SCANLINE * 153 + 2
	jr z,_
	ld l,a
	ld h,CYCLES_PER_SCANLINE
	mlt hl
_
	ld.sis (current_lyc_target_count),hl
	
	ld hl,(iy-ioregs+WY)
	ld a,l
	ld (WY_smc),a
	ld a,h
	ld (WX_smc_2),a
	cp 167
	inc a
	ld (WX_smc_3),a
	jr c,_
	ld a,$18 ;JR (overriding default JR C)
	ld (WX_smc_1),a
_
	
	call prepare_next_frame
	ld hl,(curr_palettes)
	call update_palettes_always
	
	ACALL(SetScalingMode)
	
	call flush_code_reset_padding

	ld.s de,(iy-state_size+STATE_REG_PC)
	call lookup_code
	push.s ix

	 ld.s bc,(iy-state_size+STATE_REG_BC)
	 ld.s de,(iy-state_size+STATE_REG_DE)
	 ld.s hl,(iy-state_size+STATE_REG_HL)
	 exx
	 ld de,(iy-state_size+STATE_REG_AF)
	 ld h,flags_lut >> 8
	 ld l,e
	 ld.s e,(hl)
	 push de
	 pop af
	
	 ld hl,(iy-state_size+STATE_FRAME_COUNTER)
	 ld.sis (frame_cycle_target),hl
	
	 ld.s hl,(iy-state_size+STATE_REG_SP)
	 ld bc,(CALL_STACK_DEPTH+1)*256
	 ld ix,trigger_event_startup
	 jp set_gb_stack
	
ExitEmulation:
	call.il get_event_gb_address
	ld ix,state_start+state_size
	ld (ix-state_size+STATE_REG_PC),hl
	
	exx
	ld de,(z80codebase+sp_base_address)
	xor a
	ld (ix-state_size+STATE_SYSTEM_TYPE),a
	sbc hl,de
	ld.s (ix-state_size+STATE_REG_SP),hl
	
	ex af,af'
	push af
	pop hl
	ld h,flags_lut >> 8
	set 3,l
	ld.s l,(hl)
	ld h,a
	ld (ix-state_size+STATE_REG_AF),hl
	
	xor a
	ld b,a
	sbc hl,hl
	add.s hl,sp
	lea de,ix-state_size+STATE_REG_BC
	ld c,6
	ldir.s
	
	dec bc
	ld a,(z80codebase+event_cycle_count)
	ld c,a
	lea hl,iy
	add hl,bc
	ex de,hl
	
	ld.sis hl,(frame_cycle_target)
	add.s hl,de
	jr c,_
	ld bc,CYCLES_PER_FRAME
	add hl,bc
_
	ld (ix-state_size+STATE_FRAME_COUNTER),hl
	
	ld.sis hl,(serial_cycle_count)
	or a
	sbc hl,de
	ld (ix-state_size+STATE_SERIAL_COUNTER),hl
	
	ld.sis hl,(div_cycle_count)
	add hl,de
	ld (ix-state_size+STATE_DIV_COUNTER),hl
	
	ld a,(ix-ioregs+TAC)
	and 4
	jr z,+++_
	ld.sis de,(timer_cycle_target)
	sbc hl,de
	ld a,(z80codebase+updateTIMA_smc)
	sub 6
	jr z,++_
_
	add hl,hl
	inc a
	jr nz,-_
_
	ld (ix-ioregs+TIMA),h
_
	
	ld a,(z80codebase+intstate)
	ld (ix-state_size+STATE_INTERRUPTS),a
	
	ld a,(z80codebase+curr_rom_bank)
	ld (ix-state_size+STATE_ROM_BANK),a
	
	ld a,(ram_size)
	dec a
	ld a,(mbc_rtc_last_latch)
	jr nz,_
	ld a,(z80codebase+mbc1_ram_smc)
	sub $28
	rlca
_
	and 1
	ld (ix-state_size+STATE_MBC_MODE),a
	
	ld a,(cram_bank_base+2)
	cp z80codebase >> 16
	jr nz,_
	ld a,(cram_bank_base)
	sub (rtc_latched-8)&$FF
	jr ++_
_
	ld a,(cram_bank_base+1)
	ld hl,(z80codebase+cram_base_0)
	sub h
	rlca
	rlca
	rlca
	and 3
_
	ld (ix-state_size+STATE_RAM_BANK),a
	
	; Zero-fill the rest
	lea hl,ix-state_size+STATE_END
	lea de,ix-state_size+STATE_END+1
	ld bc,state_size-STATE_END-1
	ld (hl),b
	ldir
	
	; Copy into the state file
	ld l,c
	ld de,hram_saved
	ld b,$0200 >> 8
	ldir
	
ExitEmulationWithoutState:
	; Handle RTC saving
	ld a,(ram_size)
	dec a
	jr z,++_
	call update_rtc
	ld ix,ram_size-46
	ld bc,(ix+46)
	add ix,bc
	sbc hl,hl
	ld de,z80codebase+rtc_latched
	ld b,10
_
	ld a,(de)
	ld (ix),a
	ld (ix+1),hl
	inc de
	lea ix,ix+4
	djnz -_
	push ix
	 ACALL(GetUnixTimeStamp)
	 ex de,hl
	pop hl
	ld (hl),ix
	inc hl
	inc hl
	inc hl
	ld (hl),de
	inc hl
	inc hl
	inc hl
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
_
	ld sp,(saveSP)
	ld hl,vRam
	push hl
	pop de
	inc de
	ld bc,320*240*2-1
	ld (hl),$FF
	ldir
	ld a,$D0
	ld mb,a
	ld a,2
	ld (mpKeypadScanMode),a
	xor a
	ld (mpLcdImsc),a
	ACALL(SetDefaultLCDWindowAndTiming)
	pop hl
	ld (mpLcdCtrl),hl
	pop hl
	ld (mpLcdBase),hl
	pop hl
	ld (mpIntLatch),hl
	pop hl
	ld (mpIntEnable),hl
	pop af
	pop hl
	ld (hl),a
	pop iy
	ld a,(exitReason)
	dec a
	srl a
	jr z,+++_
	jr c,_
	rra
	ccf
	jr c,+++_
	AJUMP(RestartFromHere)
_
	ld a,(main_menu_selection)
	cp 3
	jr nz,_
	ACALL(RestoreHomeScreen)
	ACALL_SAFERET(SaveStateFile)
	jr c,++_
_
	ACALL(LoadStateFile)
	AJUMP(StartFromHere)
_
	push af
	 ACALL(RestoreHomeScreen)
	 ACALL_SAFERET(SaveRAM)
	pop af
	ret
	
LoadConfigFile:
	ld hl,ConfigFileName
	ACALL(LookUpAppvar)
	ret c
	
	; Check the version byte
	ld a,(hl)
	inc hl
	dec a
	ret nz
	
	ld de,FrameskipValue
	ld bc,0
	ld c,(hl)
	inc hl
	ld a,c
	dec a
	cp option_config_count
	ret nc
	ldir
	
	ld de,KeyConfig
	ld c,(hl)
	inc hl
	ld a,c
	dec a
	cp key_config_count
	ret nc
	ldir
	ret
	
SaveConfigFile:
	ld hl,ConfigFileName
	push hl
	 ACALL(LookUpAppvar)
	 jr c,_
	
	 dec hl
	 dec hl
	 inc bc
	 inc bc
	 ld de,config_start
	 call memcmp
	pop hl
	ret z
	
	push hl
	 call _Mov9ToOP1
	 call _chkFindSym
	 call nc,_DelVarArc
_
	pop hl
	push hl
	 call _Mov9ToOP1
	 call _CmpPrgNamLen
	 ld bc,0
	 call _CreatePVar4
	 push hl
	 pop ix
	 ld hl,(config_start << 16) | (config_start & $00FF00) | (config_start >> 16)
	 ld (ix-5),hl
	 
	 ld hl,config_start - userMem
	 ld (asm_prgm_size),hl
	pop hl
	call _Mov9ToOP1
	call Arc_Unarc_Safe	; Must be CALL due to special return address handling
	ret
	
SaveStateFile:
	ld hl,ROMName
	push hl
_
	 inc hl
	 ld a,(hl)
	 or a
	 jr nz,-_
	 dec hl
	 ld a,(current_state)
	 add a,'0'
	 ld (hl),a
	 dec hl
	 ld (hl),'t'
	 dec hl
	 ld (hl),'S'
	 ex (sp),hl
	 ; Delete the existing variable
	 push hl
	  call _Mov9ToOP1
	  call _chkFindSym
	  call nc,_DelVarArc
	 pop hl
	 push hl
	  call _Mov9ToOP1
	  call _CmpPrgNamLen
	  ld bc,0
	  call _CreatePVar4
	  push hl
	  pop ix
	  ld hl,(save_state_size << 16) | (save_state_size & $00FF00) | (save_state_size >> 16)
	  ld (ix-5),hl
	 pop hl
	 call _Mov9ToOP1
	 call Arc_Unarc_Safe
	pop hl
	ld (hl),0
	; Reindex the ROM in case a Garbage Collect occurred, and reinsert the RAM.

LoadROMAndRAM:
	ld hl,ROMName+1
	ld (errorArg),hl
	ACALL_SAFERET(LoadROM)
	ret c
	
	ld ix,rombankLUT
	ld hl,(ix)
	ld e,3
	mlt de
	add ix,de
	
	ld de,$4000
	add hl,de
	ld (rom_start),hl
	ld bc,$0147
	add hl,bc
	ld a,(hl)
	ld c,a
	dec b
	ld (errorArg),bc
	or a
	jr z,mbc_valid_no_carry
	inc b	;MBC1
	dec a
	cp $04-$01
	jr c,mbc_valid
	inc b	;MBC2
	sub $05-$01
	cp $07-$05
	jr c,mbc_valid
	inc b
	inc b	;MBC3+RTC
	sub $0F-$05
	sub $11-$0F
	jr c,mbc_valid
	dec b	;MBC3
	cp $14-$11
	jr c,mbc_valid
	;MBC5
	sub $19-$11
	cp $1F-$19
mbc_valid:
	ccf
mbc_valid_no_carry:
	ld a,ERROR_UNSUPPORTED_MBC
	ret c
	ld a,b
	ld (mbc),a
	
	; Mirror ROM across all banks
	lea de,ix
	ld hl,rombankLUT_end
	sbc hl,de
	jr z,_
	ld a,ERROR_INVALID_ROM
	ret c
	ld a,b
	push hl
	pop bc
	ld hl,rombankLUT
	ldir
_
	
	; Handle MBC-specific mirrors
	ld hl,(rombankLUT+3)
	ld (rombankLUT),hl
	dec a	;MBC1
	jr nz,_
	ld hl,(rombankLUT+($21*3))
	ld (rombankLUT+($20*3)),hl
	ld hl,(rombankLUT+($41*3))
	ld (rombankLUT+($40*3)),hl
	ld hl,(rombankLUT+($61*3))
	ld (rombankLUT+($60*3)),hl
_
	
	ld hl,save_state_prefix_size + 2
	call _EnoughMem
	ret c
	ex de,hl
	ld de,save_state_size
	push de
	 push hl
	  call _InsertMem
	 pop bc
	pop hl
	push hl
	pop de
	inc de
	push hl
	 push bc
	  dec bc
	  ld (hl),0
	  ldir
	
	  ACALL(LoadRAM)
	 pop de
	 jr nc,_
	pop hl
	push af
	 call _DelMem
	pop af
	ret
_
	 ld hl,(ram_size)
	 add hl,de
	 ex de,hl
	pop hl
	ld (hl),de
	ret
	
LoadROM:
	ld hl,ROMName
	ACALL(LookUpAppvar)
	ld a,ERROR_FILE_MISSING
	ret c
	ld a,c
	sub 9
	ld c,a
	jr nc,_
	ld a,b
	or a
	ld a,ERROR_FILE_INVALID
	ret z
	dec b
_
	push bc
	 ld de,MetaHeader
	 ld bc,8
	 call memcmp
	pop bc
	ld a,ERROR_FILE_INVALID
	scf
	ret nz
	ld d,(hl)
	dec d
	ret m
	inc d
	inc hl
	ld (current_description),hl
	
	ld hl,ROMName
_
	inc hl
	ld a,(hl)
	or a
	jr nz,-_
	ld (hl),'R'
	inc hl
LoadROMRestartLoop:
	ld (hl),'0'
	inc hl
	ld (hl),'0'
	inc hl
	ld (hl),a
	
	ld a,d
	ld hl,rombankLUT
	push hl
	pop de
	inc de
	ld bc,128*3 - 1
	ld (hl),$FF
	ldir
	ld d,a
	ld e,a
LoadROMLoop:
	push de
	 ld hl,ROMName
	 ACALL_SAFERET(LookUpAppvar)
	pop de
	ld a,ERROR_FILE_MISSING
	ret c
	push hl
	 add hl,hl
	pop hl
	jr nc,LoadROMPageLoop
	push de
	 call Arc_Unarc_Safe
	pop de
	ld hl,ROMName
_
	inc hl
	ld a,(hl)
	or a
	jr nz,-_
	dec hl
	dec hl
	jr LoadROMRestartLoop

LoadROMPageLoop:
	ld a,(hl)
	cp d
	ccf
	ld a,ERROR_FILE_INVALID
	ret c
	push de
	 ld e,(hl)
	 ld d,3
	 mlt de
	 ld ix,rombankLUT
	 add ix,de
	 bit 7,(ix+2)
	 scf
	 jr z,_
	 inc hl
	 dec bc
	 push bc
	  ld c,(hl)
	  inc hl
	  ld b,(hl)
	  inc hl
	  ld de,-$4000
	  add hl,de
	  ld (ix),hl
	  add hl,bc
	  sbc hl,de
	  ex (sp),hl
	  inc bc
	  sbc hl,bc
	  ex (sp),hl
	 pop bc
_
	pop de
	ret c
	jr z,_
	dec e
	jr nz,LoadROMPageLoop
	scf
	ret
_
	dec e
	ret z
	ld hl,ROMName
_
	inc hl
	ld a,(hl)
	or a
	jr nz,-_
	dec hl
	inc (hl)
	ld a,(hl)
	cp '9'+1
	jr c,LoadROMLoop
	ld (hl),'0'
	dec hl
	inc (hl)
	jr LoadROMLoop
	
LoadRAM:
	ld hl,ROMName
_
	inc hl
	ld a,(hl)
	or a
	jr nz,-_
	dec hl
	ld (hl),'V'
	dec hl
	ld (hl),'A'
	dec hl
	ld (hl),'S'
	
	ld de,8*1024
	ld a,(mbc)
	cp 2	;MBC2
	jr z,+++_
	cp 4	;MBC3+RTC
	jr nz,_
	ld e,48
_
	ld hl,(rom_start)
	ld bc,$0149
	add hl,bc
	ld a,(hl)
	or a
	jr nz,_
	ld d,a
_
	cp 3
	jr c,_
	ld d,(32*1024) >> 8
_
	
	push de
	 ld a,d
	 or e
	 jr z,LoadRAMNoVar
	 ld b,d
	 ld c,e
	 ld hl,vram_tiles_start
	 ld (hl),l
	 push hl
	 pop de
	 inc de
	 dec bc
	 ldir
_
	 ld hl,ROMName
	 ACALL(LookUpAppvar)
	 jr nc,_
	 or a
	 sbc hl,hl
	 call _createAppVar
	 jr -_
_
	 push bc
	  push hl
	   ; Only load compression type 0
	   ld a,(hl)
	   or a
	   jr nz,_
	   inc hl
	   dec bc
	   ld a,b
	   or c
	   jr z,_
	   ld de,vram_tiles_start
	   ldir
_
	  pop de
	 pop hl
	 call _ChkInRAM
	 jr nz,LoadRAMNoVar
	 ex de,hl
	 dec hl
	 ld (hl),b
	 dec hl
	 ld (hl),c
	 inc hl
	 inc hl
	 call _DelMem
LoadRAMNoVar:
	pop hl
	inc hl
	push hl
	 inc hl
	 inc hl
	 call _EnoughMem
	pop hl
	ld a,ERROR_NOT_ENOUGH_MEMORY
	ret c
	push hl
	 ex de,hl
	 ld de,ram_size
	 call _InsertMem
	pop bc
	ex de,hl
	; Compression type is implicitly set to 0 here
	ld (hl),bc
	inc hl
	inc hl
	inc hl
	ld (cram_start),hl
	dec bc
	ld a,b
	or c
	jr z,_
	ex de,hl
	ld hl,vram_tiles_start
	ldir
	ret
_
	ld hl,mpZeroPage
	ld (cram_start),hl
	ret
	
	

SaveRAM:
	ld hl,ram_size
	ld bc,(hl)
	ld a,c
	dec a
	or b
	jr z,SaveRAMDeleteMem

	inc bc
	inc bc
	call checksum
	ld (cart_ram_checksum),ix
	
	ld hl,ROMName
	push hl
_
	 inc hl
	 ld a,(hl)
	 or a
	 jr nz,-_
	 dec hl
	 ld (hl),'V'
	 dec hl
	 ld (hl),'A'
	 dec hl
	 ld (hl),'S'
	pop hl
	ACALL(LookUpAppvar)
	jr c,SaveRAMRecreate
	
	push de
	 dec hl
	 dec hl
	 inc bc
	 inc bc
	 ld de,ram_size
	 call memcmp
	pop hl
	jr z,SaveRAMDeleteMem
	
	; Delete the existing variable
	push hl
	pop ix
	ld de,(ix-7)
	ld d,(ix-4)
	ld e,(ix-3)
	call _DelVarArc
	
SaveRAMRecreate:
	ld hl,ROMName
	call _Mov9ToOP1
	call _CmpPrgNamLen
	ld bc,0
	call _CreatePVar4
	push hl
	pop ix
	ld hl,(ram_size << 16) | (ram_size & $00FF00) | (ram_size >> 16)
	ld (ix-5),hl
	
	scf
	jr SaveAutoState
	
SaveRAMDeleteMem:
	ld hl,ram_size
	ld de,(hl)
	inc de
	inc de
	call _DelMem
	or a
	
SaveAutoState:
	push af
	 ld hl,ROMName
	 push hl
_
	  inc hl
	  ld a,(hl)
	  or a
	  jr nz,-_
	  dec hl
	  ld (hl),'A'
	  dec hl
	  ld (hl),'t'
	 pop hl
	 call _Mov9ToOP1
	 call _chkFindSym
	 call nc,_DelVarArc
	
	 ld hl,AutoSaveState
	 ld a,(hl)
	 res 1,(hl)
	 dec a
	 ld hl,save_state_size
	 ld de,save_state_prefix_size
	 jr nz,SaveAutoStateDeleteMem
	 
	 ld (hl),de
	 
	 ld hl,ROMName
	 call _Mov9ToOP1
	 call _CmpPrgNamLen
	 ld bc,0
	 call _CreatePVar4
	 push hl
	 pop ix
	 ld hl,(save_state_size << 16) | (save_state_size & $00FF00) | (save_state_size >> 16)
	 ld (ix-5),hl
	
	 ld hl,ROMName
	 call _Mov9ToOP1
	 call Arc_Unarc_Safe	; Must be CALL due to special return address handling
	 jr ArchiveSaveRAM
	
SaveAutoStateDeleteMem:
	 inc de
	 inc de
	 call _DelMem

ArchiveSaveRAM:
	pop af
	ret nc

	ld hl,ROMName
	push hl
_
	 inc hl
	 ld a,(hl)
	 or a
	 jr nz,-_
	 dec hl
	 ld (hl),'V'
	 dec hl
	 ld (hl),'A'
	pop hl
	call _Mov9ToOP1
	call Arc_Unarc_Safe	; Must be CALL due to special return address handling
	ret
	
LoadStateFile:
	ld hl,ROMName
	push hl
_
	 inc hl
	 ld a,(hl)
	 or a
	 jr nz,-_
	 dec hl
	 ld a,(current_state)
	 add a,'0'
	 ld (hl),a
	 dec hl
	 ld (hl),'t'
	 dec hl
	 ld (hl),'S'
	pop hl
	
	ACALL(LookUpAppvar)
	ret c
	
	ex de,hl
	ld a,(current_state)
	add a,-10
	jr c,LoadStateFileWithoutRAM
	
	ld hl,(save_state_size)
	sbc hl,bc
	scf
	ret nz
	
	ld hl,save_state_prefix_size
	add hl,de
	push de
	 ld de,(hl)
	 ld hl,(ram_size)
	 sbc hl,de
	pop hl
_
	scf
	ret nz
	
	ld de,save_state_size+2
	ldir
	or a
	ret
	
LoadStateFileWithoutRAM:
	push bc
	 ld hl,ram_size
	 ld bc,(hl)
	 ld a,c
	 dec a
	 or b
	 jr z,_
	 push de
	  inc bc
	  inc bc
	  call checksum
	 pop de
	 ld hl,cart_ram_checksum - (save_state_size + 2)
	 add hl,de
	 ld bc,(hl)
	 lea hl,ix
	 sbc hl,bc
_
	pop bc
	scf
	ret nz

	; Carry is set
	ld hl,save_state_prefix_size + 1
	sbc hl,bc
	ex de,hl
	jr --_
	
LookUpAppvar:
	call _Mov9ToOP1
	call _chkFindSym
	ret c
GetDataSection:
	call _ChkInRAM
	ex de,hl
	ld bc,9
	jr z,_
	add hl,bc
	ld c,(hl)
	add hl,bc
	inc hl
_
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	or a
	ret
	
	
ROMSearch:
	xor a
	sbc hl,hl
	ld (penCol),hl
	ld (penRow),a
	ld hl,(progPtr)
	or a
ROMSearchLoop:
	ld (ix),hl
	ex de,hl
	ld hl,(pTemp)
	sbc hl,de
	ret z
	ld a,(de)
	ld hl,-7
	add hl,de
	ld de,(hl)
	xor appVarObj
	jr nz,NoMatch
	push hl
	 inc hl
	 inc hl
	 inc hl
	 ld d,(hl)
	 inc hl
	 ld e,(hl)
	 ACALL(GetDataSection)
	 ld de,MetaHeader
	 ld bc,8
	 call memcmp
	 jr nz,_
	 lea ix,ix+3
_
	pop hl
	ld de,(hl)
NoMatch:
	ld e,1
	mlt de
	sbc hl,de
	jr ROMSearchLoop
	
	
DrawMenuItem:
	ld de,-7
	add hl,de
	ld de,(hl)
	inc hl
	inc hl
	inc hl
	ld d,(hl)
	inc hl
	ld e,(hl)
	ACALL(GetDataSection)
	ld de,9
	add hl,de
	ld b,(hl)
	inc hl
	jp _VPutSN
	
	; Input: rtc_last
	; Output: HLIX = 48-bit timestamp based on current time
GetUnixTimeStamp:
	ld.sis ix,(rtc_last+3)
	ld de,(epochDayCount)
	add ix,de
	or a
	sbc hl,hl
	call MulHLIXBy24
	ld.sis bc,(rtc_last+2)
	ld b,0
	add ix,bc
	jr nc,_
	inc hl
_	
	
	call MulHLIXBy24
	add ix,ix \ adc hl,hl
	add ix,bc \ adc hl,de
	ld.sis bc,(rtc_last+1)
	ld b,0
	add ix,bc
	jr nc,_
	inc hl
_
	
	call MulHLIXBy24
	add ix,ix \ adc hl,hl
	add ix,bc \ adc hl,de
	ld.sis bc,(rtc_last)
	ld b,0
	add ix,bc
	jr nc,_
	inc hl
	or a
_
	
	push hl
	 sbc hl,hl
	 ld de,(timeZoneOffset)
	 sbc hl,de
	 ex de,hl
	pop hl
	ret z
	
	add ix,de
	jr c,_
	dec hl
_
	
	ret m
	inc hl
	ret
	
	
	; Input: HLIX = 48-bit UTC timestamp
	; Output: A,B,C = hours,minutes,seconds
	;         HLIX = days
ExtractUnixTimeStamp:
	ld de,(timeZoneOffset)
	add ix,de
	jr nc,_
	inc hl
_
	ex de,hl
	add hl,hl
	ex de,hl
	jr nc,_
	dec hl
_
	call DivHLIXBy60
	ld c,a
	push bc
	 call DivHLIXBy60
	pop bc
	ld b,a
	push bc
	 ld c,24
	 call DivHLIXByC
	pop bc
	ret
	
	; Out: 12 bytes of timing on the stack
GetLCDTiming:
	pop ix
	ld hl,(mpLcdTiming0+9)
	push hl
	ld hl,(mpLcdTiming0+6)
	push hl
	ld hl,(mpLcdTiming0+3)
	push hl
	ld hl,(mpLcdTiming0)
	push hl
	jp (ix)
	
	; In: 12 bytes of timing on the stack
SetDefaultLCDWindowAndTiming:
	xor a
	ld de,319
	ld hl,239
	
	; In: 12 bytes on timing on the stack
	;     A = left side of window
	;     DE = right side of window
	;     H = top side of window
	;     L = bottom side of window
SetLCDWindowAndTiming:
	push hl
	 ld b,3
_
	 ld hl,mpLcdIcr
	 ld (hl),4
	 ld l,mpLcdRis & $FF
_
	 bit 2,(hl)
	 jr z,-_
	 djnz --_
	 ld l,mpLcdCtrl & $FF
	 res 0,(hl)
	 push af
	  ld a,$2A
	  call spiCmd
	  call spiParam
	 pop af
	 call spiParam
	 ld a,d
	 call spiParam
	 ld a,e
	 call spiParam
	pop de
	ld a,$2B
	call spiCmd
	call spiParam
	ld a,d
	call spiParam
	call spiParam
	ld a,e
	call spiParam
	ld a,$2C
	call spiCmd
	
	pop ix
	pop hl
	ld (mpLcdTiming0),hl
	pop hl
	ld (mpLcdTiming0+3),hl
	pop hl
	ld (mpLcdTiming0+6),hl
	pop hl
	ld (mpLcdTiming0+9),hl
	
	ld hl,mpLcdCtrl
	set 0,(hl)
	jp (ix)
	
SetScalingMode:
	ld a,(ScalingMode)
	or a
	ld a,$33
	jr nz,_
	ld a,$3
_	
	ld (scaling_mode_smc_1),a
	and $11
	ld (scaling_mode_smc_2),a
	ld c,a
	dec a
	ld b,WHITE
	mlt bc
	ld a,c
	ld (scaling_mode_smc_3),a
	
	ld ix,scanlineLUT_1
	ld hl,gb_frame_buffer_1
	ld de,160
	jr z,SetNoScalingMode
	
	ld b,144/3*2
_
	ld (ix),hl
	add hl,de
	add hl,de
	ld (ix+3),hl
	add hl,de
	add hl,de
	ld (ix+6),hl
	add hl,de
	lea ix,ix+9
	djnz -_
	
	ld hl,vram_pixels_start
	ld c,$6000 >> 8
_
	ld a,(hl)
	rld
	inc hl
	djnz -_
	dec c
	jr nz,-_
	
	ld a,2
	ACALL(generate_digits)
	
Set4BitWindow:
	ld hl,$00EF78
	push hl
	ld hl,$01043B
	push hl
	ld hl,$093F1F
	push hl
	ld hl,$460338
	push hl
	ACALL(SetDefaultLCDWindowAndTiming)
	
	ld hl,$010C25
	ld (mpLcdCtrl),hl
	ret
	
SetNoScalingMode:
	ld c,2
_
	ld b,144
_
	ld (ix),hl
	add hl,de
	lea ix,ix+3
	djnz -_
	ld hl,gb_frame_buffer_2
	dec c
	jr nz,--_
	
	ld hl,vram_pixels_start
	ld c,$6000 >> 8
_
	ld a,(hl)
	inc a
	and $0F
	dec a
	ld (hl),a
	inc hl
	djnz -_
	dec c
	jr nz,-_
	
	ld a,4
	ACALL(generate_digits)
	
	ld a,(SkinDisplay)
	rra
	jr nc,_
	ld hl,(skin_file_ptr)
	ex de,hl
	sbc hl,hl
	add hl,de
_
	ld hl,(mpLcdBase)
	push hl
	 jr nc,no_skin
	 push de
	  ld hl,mpLcdPalette
	  ld de,palette_backup
	  ld bc,32
	  push hl
	   ldir
	  pop de
	 pop hl
	 ld c,32
	 ldir
	pop de
	ld b,160*240/256
	
rle_decode:
	ld a,(hl)
	cp $10
	jr c,rle_literal
	 
	rlca
	rlca
	rlca
	rlca
	ld ixh,a
	and $0F
	inc a
	ld ixl,a
	ld a,ixh
	xor (hl)
	and $F0
	xor (hl)
	ex de,hl
_
	ld (hl),a
	cpi
	jp po,rle_done
	dec ixl
	jr nz,-_
	ex de,hl
	inc hl
	jr rle_decode
	
rle_literal:
	inc a
	inc hl
_
	ldi
	jp po,rle_done
	dec a
	jr nz,-_
	jr rle_decode
	
no_skin:
	pop de
	inc de
	ld (hl),BLACK_BYTE
	ld bc,160*240-1
	ldir
	
Set8BitWindow:
	ld hl,$011F78
	push hl
	ld hl,$0200F0
	push hl
	ld hl,$084F0D
	push hl
	ld hl,$040344
	push hl
	ld a,80
	ld de,239
	ld hl,48*256 + 191
	ACALL(SetLCDWindowAndTiming)
	
	ld hl,$010C27
	ld (mpLcdCtrl),hl
	ret
	
IdentifyDefaultPalette:
	ld ix,(rom_start)
	ld bc,$0130
	add ix,bc
	dec b	;B=0
	ld a,(ix-$30+$4B)	; Old Licensee Code
	cp $33
	jr nz,old_licensee
	ld a,(ix-$30+$44)	; New Licensee Code byte 1
	cp c
	jr nz,no_special_palette
	ld a,(ix-$30+$45)	; New Licensee Code byte 2
	sub c
old_licensee:
	dec a
	jr nz,no_special_palette
	lea hl,ix-$30+$34	; Game title
	ld b,$10
title_checksum_loop:
	add a,(hl)
	inc hl
	djnz title_checksum_loop
	APTR(DefaultPaletteChecksumTable)
	ld c,$4F
	cpir
	jr nz,no_special_palette
	ld a,$4F-1
	sub c
	cp $41
	jr c,default_palette_found
	dec hl
	ld c,$4F-$41
title_second_checksum_loop:
	ld e,a
	add hl,bc
	ld a,(ix-$30+$37)	; 4th character of game title
	cp (hl)
	ld a,e
	jr z,default_palette_found
	add a,c
	cp $5E
	jr c,title_second_checksum_loop
	
no_special_palette:
	xor a
default_palette_found:
	ld c,a
	APTR(DefaultPaletteIndexTable)
	add hl,bc
	ld a,(hl)
	ld (default_palette),a
	ret
	
regs_init:
	.db $00	; Hardware type
	.db $00 ; Interrupt enable
	;     AF,   BC,   DE,   HL,   SP,   PC
	.dw $01B0,$0013,$00D8,$014D,$FFFE,$0100
	.dw $0000 ; Frame cycle counter
	.dw $0000 ; Serial transfer cycle counter
	.dw $0000 ; Divisor cycle counter
	.db $01 ; Cart ROM bank
	.db $00 ; Cart RAM bank
	.db $00 ; MBC mode
	
	
hmem_init:
	.db 0,0,0,0,0,$00,$00,$00,0,0,0,0,0,0,0,0
	.db $80,$BF,$F3,0,$BF,0,$3F,$00,0,$BF,$7F,$FF,$9F,0,$BF,0
	.db $FF,$00,$00,$BF,$77,$F3,$F1,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.db $91,0,$00,$00,0,$00,0,$FC,$FF,$FF,$00,$00,0,0,0,0
	.block $b0
	
flags_lut_init:
	.db $00,$00,$00,$00,$00,$00,$00,$00, $00,$10,$40,$50,$00,$10,$40,$50
	.db $01,$01,$01,$01,$01,$01,$01,$01, $20,$30,$60,$70,$20,$30,$60,$70
	.db $10,$10,$10,$10,$10,$10,$10,$10, $00,$10,$40,$50,$00,$10,$40,$50
	.db $11,$11,$11,$11,$11,$11,$11,$11, $20,$30,$60,$70,$20,$30,$60,$70
	.db $02,$02,$02,$02,$02,$02,$02,$02, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $03,$03,$03,$03,$03,$03,$03,$03, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $12,$12,$12,$12,$12,$12,$12,$12, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $13,$13,$13,$13,$13,$13,$13,$13, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $40,$40,$40,$40,$40,$40,$40,$40, $00,$10,$40,$50,$00,$10,$40,$50
	.db $41,$41,$41,$41,$41,$41,$41,$41, $20,$30,$60,$70,$20,$30,$60,$70
	.db $50,$50,$50,$50,$50,$50,$50,$50, $00,$10,$40,$50,$00,$10,$40,$50
	.db $51,$51,$51,$51,$51,$51,$51,$51, $20,$30,$60,$70,$20,$30,$60,$70
	.db $42,$42,$42,$42,$42,$42,$42,$42, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $43,$43,$43,$43,$43,$43,$43,$43, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $52,$52,$52,$52,$52,$52,$52,$52, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $53,$53,$53,$53,$53,$53,$53,$53, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	
DefaultPaletteChecksumTable:
	.db $00,$88,$16,$36,$D1,$DB,$F2,$3C,$8C,$92,$3D,$5C,$58,$C9,$3E,$70
	.db $1D,$59,$69,$19,$35,$A8,$14,$AA,$75,$95,$99,$34,$6F,$15,$FF,$97
	.db $4B,$90,$17,$10,$39,$F7,$F6,$A2,$49,$4E,$43,$68,$E0,$8B,$F0,$CE
	.db $0C,$29,$E8,$B7,$86,$9A,$52,$01,$9D,$71,$9C,$BD,$5D,$6D,$67,$3F
	.db $6B,$B3,$46,$28,$A5,$C6,$D3,$27,$61,$18,$66,$6A,$BF,$0D,$F4
SecondPaletteChecksumTable:
	.db $42,$45,$46,$41,$41,$52,$42,$45,$4B,$45,$4B,$20,$52,$2D
	.db $55,$52,$41,$52,$20,$49,$4E,$41,$49,$4C,$49,$43,$45,$20
	.db $52
	
DefaultPaletteIndexTable:
	; Dummy entry was formerly $7C, but grayscale is better
	.db $16,$08,$12,$A3,$A2,$07,$87,$4B,$20,$12,$65,$A8,$16
	.db $A9,$86,$B1,$68,$A0,$87,$66,$12,$A1,$30,$3C,$12,$85
	.db $12,$64,$1B,$07,$06,$6F,$6E,$6E,$AE,$AF,$6F,$B2,$AF
	.db $B2,$A8,$AB,$6F,$AF,$86,$AE,$A2,$A2,$12,$AF,$13,$12
	.db $A1,$6E,$AF,$AF,$AD,$06,$4C,$6E,$AF,$AF,$12,$7C,$AC
	.db $A8,$6A,$6E,$13,$A0,$2D,$A8,$2B,$AC,$64,$AC,$6D,$87
	.db $BC,$60,$B4,$13,$72,$7C,$B5,$AE,$AE,$7C,$7C,$65,$A2
	.db $6C,$64,$85
	
#macro DEFINE_ERROR(name, text)
	#define NUM_ERRORS eval(NUM_ERRORS+1)
	clr()
	wr(name,"=NUM_ERRORS")
	run()
	.db text,0
#endmacro

#define NUM_ERRORS 0
error_text:
	.db "Error: ",0
	DEFINE_ERROR("ERROR_FILE_MISSING", "Missing AppVar %s")
	DEFINE_ERROR("ERROR_FILE_INVALID", "Invalid AppVar %s")
	DEFINE_ERROR("ERROR_UNSUPPORTED_MBC", "Unsupported cartridge type %02X")
	DEFINE_ERROR("ERROR_INVALID_ROM", "ROM is invalid")
	DEFINE_ERROR("ERROR_NOT_ENOUGH_MEMORY", "Not enough RAM free")
	DEFINE_ERROR("ERROR_RUNTIME", "Encountered a runtime error!")
	DEFINE_ERROR("ERROR_INVALID_OPCODE", "Encountered an invalid opcode!")
	
ErrorNoROMsFound:
	.db "No ROMs found!",0
	
WebsiteURL:
	.db "https://calc84maniac.github.io/tiboyce",0
	