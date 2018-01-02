MENU_ITEM_COUNT = 16
	
	.org 0
Startup:
	ld (ArcBase),hl
	call _RunIndicOff
	
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
	call _ClrLCDFull
	ld a,32
	ld (penRow),a
	ld hl,91
	ld (penCol),hl
	ld hl,$0031
	ld.sis (drawFGColor-ramStart),hl
	APTR(EmulatorTitle)
	call _VPutS
	or a
	sbc hl,hl
	ld.sis (drawFGColor-ramStart),hl
	ld c,l
RedrawMenu:
	ld de,(menuFrame)
	ld a,36
	ld b,MENU_ITEM_COUNT
RedrawMenuLoop:
	add a,12
	ld (penRow),a
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
RedrawMenuSelectedItem:
	set textInverse,(iy+textFlags)
RedrawMenuItem:
	; Carry is clear
	sbc hl,hl
	ld (penCol),hl
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
	jr RestoreHomeScreen
	
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
	jr c,RestoreHomeScreen
	AJUMP(RepopulateMenu)
	
StartROM:
	ACALL_SAFERET(LoadROMAndRAM)
	ret c

RestartFromHere:
	ld hl,vram_start
	push hl
	pop de
	inc de
	ld bc,regs_saved - vram_start - 1
	ld (hl),0
	ldir
	
	push de
	 APTR(regs_init)
	pop de
	ld bc,hmem_init - regs_init
	ldir
	
	ld de,hram_saved + $0100
	ld bc,$0100
	ldir
	
StartFromHere:
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
	ld hl,$463B01
	ACALL(GetAndSetLCDTiming)
	push de
	
	APTR(palettecode)
	ld de,mpLcdPalette
	ld bc,palettecodesize
	ldir
	
	ld hl,$0C25
	ld (mpLcdCtrl),hl
	ld hl,gb_frame_buffer_1
	ld (mpLcdBase),hl
	push hl
	pop de
	inc de
#ifdef DBGNOSCALE
	ld bc,160*144
	ld (hl),WHITE_BYTE
	ldir
	ld bc,160*96
	ld (hl),BLUE_BYTE
	ldir
	ld bc,160*144
	ld (hl),WHITE_BYTE
	ldir
	ld bc,160*96-1
	ld (hl),BLUE_BYTE
	ldir
#else
	ld bc,320*240-1
	ld (hl),WHITE_BYTE
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
	
	APTR(CmdExit)
	ld (z80codebase+CmdExitSMC),hl
	
	ld ix,scanlineLUT_1
	ld (scanlineLUT_ptr),ix
	ld hl,gb_frame_buffer_1
	ld de,160
	ld b,144/3*2
_
	ld (ix),hl
	add hl,de \ add hl,de
	ld (ix+3),hl
	add hl,de
	ld (ix+6),hl
	add hl,de \ add hl,de
	lea ix,ix+9
	djnz -_
	
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
	  or $11
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
	
	ACALL(generate_digits)
	
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
	
	ld ix,ram_size-47
	ld bc,(ix+47)
	inc.s bc
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
	
	ld hl,(iy-ioregs+LYC)
	ld h,CYCLES_PER_SCANLINE
	mlt hl
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
	call update_palettes
	
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
	
CmdLoadSaveState:
	call.il get_event_gb_address
	ld ix,state_start+state_size
	ld (ix-state_size+STATE_REG_PC),hl
	
	exx
	ld de,(z80codebase+sp_base_address)
	or a
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
	or a
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
	
	ld hl,hram_start
	ld de,hram_saved
	ld bc,$0200
	ldir
	
	ld a,3
	
CmdExit:
	push af
	 ld a,(ram_size)
	 or a
	 jr z,++_
	 call update_rtc
	 ld ix,ram_size-47
	 ld bc,(ix+47)
	 inc.s bc
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
	pop af
	ld sp,(saveSP)
	ld hl,vRam
	push hl
	pop de
	inc de
	ld bc,320*240*2-1
	ld (hl),$FF
	ldir
	ld b,a
	ld a,$D0
	ld mb,a
	ld a,2
	ld (mpKeypadScanMode),a
	xor a
	ld (mpLcdImsc),a
	pop hl
	ACALL(SetLCDTiming)
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
	srl b
	jr z,+++_
	jr c,_
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

	
SaveStateFile:
	ld hl,ROMName
	push hl
	 inc hl
	 ld bc,9
	 xor a
	 cpir
	 dec hl
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
	  jr c,_
	  call _DelVarArc
_
	 pop hl
	 push hl
	  call _Mov9ToOP1
	  call _CmpPrgNamLen
	  ld bc,0
	  call _CreatePVar4
	  push hl
	  pop ix
	  ld hl,(program_end << 16) | (program_end & $00FF00) | (program_end >> 16)
	  ld (ix-5),hl
	 pop hl
	 call _Mov9ToOP1
	 call Arc_Unarc_Safe
	pop hl
	ld (hl),0
	; Reindex the ROM in case a Garbage Collect occurred, and reinsert the RAM.

LoadROMAndRAM:
	ACALL_SAFERET(LoadROM)
	ret c
	
	ld hl,(rombankLUT)
	ld de,$4000
	add hl,de
	ld (rom_start),hl
	ld bc,$0147
	add hl,bc
	ld a,(hl)
	ld b,0
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
	ret c
	ld a,b
	ld (mbc),a
	
	; Mirror ROM across all banks
	lea de,ix
	ld hl,rombankLUT_end
	sbc hl,de
	jr z,_
	ret c
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
	ld de,program_end
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
	call _DelMem
	scf
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
	ret c
	ld a,c
	sub 9
	ld c,a
	jr nc,_
	ld a,b
	or a
	ret z
	dec b
_
	push bc
	 ld de,MetaHeader
	 ld bc,8
	 call memcmp
	pop bc
	ret nz
	ld d,(hl)
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
	ld (hl),'0'
	inc hl
	ld (hl),'0'
	inc hl
	ld (hl),a
	
	ld e,a
	ld ix,rombankLUT
LoadROMLoop:
	push de
	 ld hl,ROMName
	 ACALL_SAFERET(LookUpAppvarForceARC)
	pop de
	ret c
	ld a,(hl)
	cp e
	scf
	ret nz
	inc hl
	dec bc
LoadROMPageLoop:
	push de
	 push bc
	  ld c,(hl)
	  inc hl
	  ld b,(hl)
	  inc hl
	  ld de,-$4000
	  add hl,de
	  ld (ix),hl
	  lea ix,ix+3
	  add hl,bc
	  sbc hl,de
	  ex (sp),hl
	  inc bc
	  sbc hl,bc
	  ex (sp),hl
	 pop bc
	pop de
	ret c
	jr z,_
	dec d
	ret z
	inc e
	jr LoadROMPageLoop
_
	dec d
	ret z
	inc e
	ld hl,ROMName+1
	xor a
	ld bc,9
	cpir
	dec hl
	dec hl
	inc (hl)
	ld a,(hl)
	cp '9'+1
	jr c,LoadROMLoop
	jr z,_
	cp 'F'+1
	jr nz,LoadROMLoop
	ld (hl),'0'
	dec hl
	inc (hl)
	ld a,(hl)
	cp '9'+1
	jr nz,LoadROMLoop
_
	ld (hl),'A'
	jr LoadROMLoop
	
LoadRAM:
	ld hl,ROMName+1
	ld bc,9
	xor a
	cpir
	dec hl
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
	push hl
	 inc hl
	 inc hl
	 call _EnoughMem
	 ret c
	 ex de,hl
	 ld de,ram_size
	 call _InsertMem
	pop bc
	ld a,c
	ld (de),a
	inc de
	ld a,b
	ld (de),a
	inc de
	ld (cram_start),de
	or c
	jr z,_
	ld hl,vram_tiles_start
	ldir
	ret
_
	ld hl,mpZeroPage
	ld (cram_start),hl
	ret
	
	

SaveRAM:
	ld hl,program_end
	push hl
	 ld de,save_state_prefix_size + 2
	 call _DelMem
	pop hl
	ld de,(hl)
	ld a,d
	or e
	jr z,SaveRAMDeleteMemLoaded
	
	ld hl,ROMName
	push hl
	 inc hl
	 ld bc,9
	 xor a
	 cpir
	 dec hl
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
	 ld de,program_end
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
	ld hl,(program_end << 16) | (program_end & $00FF00) | (program_end >> 16)
	ld (ix-5),hl
	
	ld a,(AutoArchive)
	or a
	ret z
	ld hl,ROMName
	call _Mov9ToOP1
	call Arc_Unarc_Safe	; Must be CALL due to special return address handling
	ret
	
SaveRAMDeleteMem:
	ld hl,program_end
	ld de,(hl)
SaveRAMDeleteMemLoaded:
	inc de
	inc.s de
	jp _DelMem
	
LoadStateFile:
	ld hl,ROMName
	push hl
	 inc hl
	 ld bc,9
	 xor a
	 cpir
	 dec hl
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
	ld hl,(save_state_size)
	sbc.s hl,bc
	scf
	ret nz
	
	ld hl,save_state_prefix_size
	add hl,de
	push de
	 ld de,(hl)
	 ld hl,(ram_size)
	 or a
	 sbc.s hl,de
	pop hl
	scf
	ret nz
	
	ld de,save_state_size+2
	ldir
	or a
	ret
	
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
	
	
LookUpAppvarForceARC:
	call _Mov9ToOP1
_
	call _chkFindSym
	ret c
	call _ChkInRAM
	jr nz,_
	call Arc_Unarc_Safe
	jr -_
_
	ex de,hl
	ld de,9
	add hl,de
	ld e,(hl)
	add hl,de
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ret
	
LookUpAppvarForceRAM:
	call _Mov9ToOP1
_
	call _chkFindSym
	ret c
	call _ChkInRAM
	jr z,_
	call Arc_Unarc_Safe
	jr -_
_
	ex de,hl
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
	
	; In: HLU = HFP, H = VFP, L = LcdTiming2
	; Out: DEU = old HFP, D = old VFP, E = old LcdTiming2 
GetAndSetLCDTiming:
	ld ix,mpLcdTiming0
	ld de,(ix)
	ld d,(ix+6)
	ld e,(ix+8)
	
	; In: HLU = HFP, H = VFP, L = LcdTiming2
SetLCDTiming:
	ld ix,mpLcdTiming0
	ld (ix+8),l
	ld (ix+6),h
	ld l,(ix)
	ld h,(ix+1)
	ld (ix),hl
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
	;     AF,   BC,   DE,   HL,   SP,   PC
	.dw $01B0,$0013,$00D8,$014D,$FFFE,$0100
	.dw $0000 ; Frame cycle counter
	.dw $0000 ; Divisor cycle counter
	.db $00 ; Interrupt enable
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