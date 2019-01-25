MENU_ITEM_COUNT = 15
	
	.org 0
Startup:
	ld (ArcBase),hl
	
	; Get the calculator type
	; Write IN A,(3) \ RET.L to RAM
	ld hl,$5B03DB
	ld (pixelShadow),hl
	ld a,$C9
	ld (pixelShadow+3),a
	di
	xor a
	call.is pixelShadow - ramStart
	and 1
	ld (calcType),a
	ei
	
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
	
	ACALL(BackupOriginalHardwareSettings)
	; Backup original LCD settings
	ld hl,mpLcdTiming0
	ld de,originalLcdSettings
	ld bc,12
	ldir
	ld hl,(mpLcdCtrl)
	ex de,hl
	ld (hl),de
	
	ld hl,GlobalErrorHandler
	call _PushErrorHandler
	
NoRomMenuLoop:
	; Set custom hardware settings
	ACALL(SetCustomHardwareSettings)
	
	; Set current description and main menu selection
	APTR(NoRomLoadedDescription)
	ld (current_description),hl
	ld a,6
	ld (main_menu_selection),a
	
	; Reset exit reason, ROM name, and state index
	xor a
	ld (exitReason),a
	ld (ROMName+1),a
	ld (current_state),a
	; Start with Load ROM menu
	inc a
	ACALL(emulator_menu)

	; If not loading a new ROM, exit
	ld a,(exitReason)
	dec a
	jr nz,SaveConfigAndQuit
	
	ACALL(RestoreOriginalHardwareSettings)
	ei
LoadNewGameLoop:
	; Copy the name from ROMNameToLoad
	ld hl,ROMName+1
	push hl
	pop de
	ld bc,9
	add hl,bc
	ldir
	
	ACALL_SAFERET(StartROM)
	; If NC is returned, we're loading another game
	jr nc,LoadNewGameLoop
	; If an error code of 0 is returned, exit
	or a
	jr z,SaveConfigAndQuit
	; Display the error, and return to the menu if ON wasn't pressed
	ACALL(DisplayError)
	jr nz,NoRomMenuLoop
	
SaveConfigAndQuit:
	call _PopErrorHandler
SaveConfigAndQuitForError:
	ACALL(RestoreOriginalHardwareSettings)
	ei
	ACALL_SAFERET(SaveConfigFile)
RestoreHomeScreen:
	; Set all palette entries to white to smooth the transition
	ld hl,mpLcdPalette
	push hl
	pop de
	inc de
	ld bc,$01FF
	ld (hl),c
	ldir
	; Clear pixelShadow etc.
	ld hl,pixelShadow
	push hl
	pop de
	inc de
	ld (hl),c
	ld bc,(8400*3) - 1
	ldir
	; Mark graph dirty
	set graphDraw,(iy+graphFlags)
	; Restore the frame buffer
	call _ClrLCDFull
	call _DrawStatusBar
	call _HomeUp
	; Change the LCD settings to fullscreen 16-bit
	AJUMP(RestoreOriginalLcdSettings)
	
	; Input: HL = insertion point
	;        DE = insertion size
	; Output: Carry set and A=error if error occurred
	;         DE = insertion point
	;         BC = insertion size
InsertMemSafe:
	push hl
	 push de
	  ; Check to make sure there's room for at least 2 VAT entries
	  ld hl,30
	  add hl,de
	  push hl
	   call _MemChk
	  pop de
	  ex de,hl
	  scf
	  sbc hl,de
	  ccf
	  inc hl
	  ld (errorArg),hl
	 pop hl
	pop de
	ld a,ERROR_NOT_ENOUGH_MEMORY
	ret c
	push hl
	 call _InsertMem
	pop bc
	; Resets carry
	ld hl,(asm_prgm_size)
	add hl,bc
	ld (asm_prgm_size),hl
	ret
	
	; Input: HL = deletion point, pointing to (fake) size bytes
	; Output: Memory is deleted according to size bytes, asm_prgm_size is adjusted
DelMemSafeSizeBytes:
	ld de,(hl)
	inc.s de
	inc de
	
	; Input:  HL = deletion point
	;         DE = deletion size
	; Output: Memory is deleted, asm_prgm_size is adjusted
DelMemSafe:
	push hl
	 ld hl,(asm_prgm_size)
	 or a
	 sbc hl,de
	 ld (asm_prgm_size),hl
	pop hl
	jp _DelMem
	
	; Input: HL = filename
	;        DE = location to convert
	;        Note: Location must have size bytes initialized and
	;              the range must end exactly on userMem + (asm_prgm_size),
	;              and also the filename should not already exist.
	; Output: Carry set, HL preserved.
	;         File is created and asm_prgm_size is updated.
ConvertMemToFile:
	push hl
#ifdef DEBUG
	 ; Assert that location + size bytes == userMem + (asm_prgm_size)
	 ex de,hl
	 ld de,(hl)
	 ex de,hl
	 inc.s hl
	 inc hl
	 add hl,de
	 ld bc,(asm_prgm_size)
	 sbc hl,bc
	 ld bc,userMem
	 sbc hl,bc
	 jr nz,$
	pop hl
	push hl
#endif
	 push de
	  call _Mov9ToOP1
	  call _CmpPrgNamLen
	  ld bc,0
	  call _CreatePVar4
	  push hl
	  pop ix
	  ld hl,2
	  add hl,sp
	  ld a,(hl)
	 pop hl
	 ld (ix-5),a
	 ld (ix-4),h
	 ld (ix-3),l
	 ld de,-userMem
	 add hl,de
	 ld (asm_prgm_size),hl
	pop hl
	ret
	
StartROMAutoStateError:
	cp ERROR_NOT_ENOUGH_MEMORY
	jr nz,RestartFromHere
	ld hl,(cram_size)
	ld de,6
	add hl,de
	ex de,hl
	ld hl,save_state_size_bytes
	ACALL(DelMemSafe)
_
	ld a,ERROR_NOT_ENOUGH_MEMORY
	scf
	ret
	
StartROMInitStateOutOfMemory:
	xor a
	ld (current_state),a
	dec de
	dec de
	ld (de),a
	dec de
	inc a
	ld (de),a
	; Temporarily prevent auto state saving because state is clean
	ld hl,AutoSaveState
	set 1,(hl)
	ld hl,(errorArg)
	push hl
	 ACALL_SAFERET(SaveStateFiles)
	pop hl
	ld (errorArg),hl
	jr -_
	
StartROM:
	ACALL_SAFERET(LoadROMAndRAM)
	ret c
	
	xor a
	ld (emulatorMessageText),a
	ld (current_state),a
	ACALL(LoadStateFiles)
	push af
	 ld a,'0'
	 ld (current_state),a
	pop af
	jr c,StartROMAutoStateError
	
	ld de,AutoStateLoadedMessage
	ACALL(SetEmulatorMessage)
	jr StartFromHere
	
RestartFromHere:
	ld hl,save_state_size_bytes
	ld de,(hl)
	ld bc,save_state_size + 1
	ld (hl),bc
	inc hl
	inc hl
	inc hl
	dec de
	dec bc
	push hl
	 push bc
	  ACALL(DelMemSafe)
	 pop de
	pop hl
	ACALL(InsertMemSafe)
	jr c,StartROMInitStateOutOfMemory
	push de
	pop hl
	inc de
	dec bc
	ld (hl),0
	ldir
	
	inc hl
	ld a,(hl)
	dec a
	inc hl
	or (hl)
	jr z,_
	inc hl
	inc hl
	ld (cram_start),hl
_
	
	APTR(regs_init)
	ld de,regs_saved
	ld bc,hmem_init - regs_init
	ldir
	
	ld de,hram_saved + $0100
	ld c,hmem_init_size
	ldir
	
	push de
	pop hl
	dec hl
	ld c,$80 - hmem_init_size + 1
	ldir
	
	ld (hl),c
	ld c,$7F
	ldir
	
StartFromHere:
	ld hl,(save_state_size_bytes)
	ld a,l
	dec a
	or h
	jr z,RestartFromHere
	
	ld a,3
	ld (main_menu_selection),a
	
	ld hl,SkinFileName
	ACALL(LookUpAppvar)
	jr nc,_
	or a
	sbc hl,hl
_
	ld (skin_file_ptr),hl
	
	ACALL(SetCustomHardwareSettings)
	
	APTR(palettecode)
	ld de,mpLcdPalette + 32
	ld bc,palettecodesize
	ldir
	
#ifdef DEBUG
	APRINTF(StartText)
#endif
	
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
	
	ld a,z80codebase >> 16
	ld mb,a
	
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
	
	ld iy,state_start+state_size
	
	ld.sis sp,myz80stack
	ld hl,ophandlerRET
	push.s hl
	
	ld a,(iy-state_size+STATE_ROM_BANK)
	ld (z80codebase+curr_rom_bank),a
	ld (z80codebase+rom_bank_check_smc_1),a
	ld (z80codebase+rom_bank_check_smc_2),a
	ld (z80codebase+rom_bank_check_smc_3),a
	ld c,a
	ld b,3
	mlt bc
	ld hl,rombankLUT
	add hl,bc
	ld hl,(hl)
	ld (rom_bank_base),hl
	
	ld a,(cram_size+1)
	and $80
	add a,$B7	; or a / scf
	ld (z80codebase+cram_size_smc),a
	
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
	
	ld ix,save_state_size_bytes - 44
	ld bc,(ix+44)
	add ix,bc
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
	ld a,(cram_size+1)
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
	rra
	jr c,_
	ld a,$AF ;XOR A (overriding AND (HL))
	ld (z80codebase+intstate_smc),a
_
	
	; Set the initial frame-relative event to one cycle in the future
	ld de,(iy-state_size+STATE_FRAME_COUNTER)
	inc.s de
	ld hl,-CYCLES_PER_FRAME
	add hl,de
	jr c,_
	ex de,hl
_
	ld.sis (frame_cycle_target),hl
	
	; Get the current DIV counter
	ld de,(iy-state_size+STATE_DIV_COUNTER)
	; Initialize timer values
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
	ld a,$20 ;JR NZ (overriding JR Z)
	ld (z80codebase+timer_enable_smc),a
_
	; Set the initial DIV-relative event to one cycle in the future
	inc de
	ld.sis (div_cycle_count),de
	
	ld a,(iy-ioregs+SC)
	cpl
	and $81
	jr nz,_
	; Set the serial counter to one cycle in the future
	ld hl,(iy-state_size+STATE_SERIAL_COUNTER)
	dec hl
	ld.sis (serial_cycle_count),hl
	ld a,$38 ;JR C (overriding JR NC)
	ld (z80codebase+serial_enable_smc),a
_
	
	lea hl,iy-ioregs+NR10
	ld ix,z80codebase + audio_port_values
	ld b,audio_port_masks - audio_port_values
_
	ld a,(hl)
	ld (ix),a
	or (ix + audio_port_masks - audio_port_values)
	ld (hl),a
	inc hl
	inc ix
	djnz -_
	
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
	jr c,_
	ld a,$C9 ;RET (overriding LD C,myspriteLY)
	ld (LCDC_1_smc),a
_
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
	
	call swap_buffers
	; Initialize the scanline LUT pointer to what prepare_next_frame expects
	ld a,h
	cp (gb_frame_buffer_1 >> 8) & $FF
	ld hl,scanlineLUT_2
	jr nz,_
	sbc hl,hl
_
	ld (scanlineLUT_ptr),hl
	call prepare_next_frame
	
	ACALL(IdentifyDefaultPalette)
	ACALL(ApplyConfiguration)
	ACALL(SetScalingMode)
	ld hl,(curr_palettes)
	call update_palettes_always
	
	call flush_code_reset_padding

	; Generate the initial code block
	ld.s de,(iy-state_size+STATE_REG_PC)
	call lookup_code
	; Get the initial GB instruction address from the block info
	ld hl,(recompile_struct+8+2)
	ld (event_gb_address),hl
	add hl,hl
	jr nc,_
	; If the GB address was in RAM, skip over the block prefix
	lea ix,ix+RAM_PREFIX_SIZE
_
	; Save the event address and write an event handler call
	lea hl,ix
	ld.sis (event_address),hl
	ld.s a,(hl)
	ld (z80codebase+event_value),a
	ld.s (hl),RST_EVENT
	
	; Get the GB registers in BDEHL'
	ld.s bc,(iy-state_size+STATE_REG_BC)
	ld.s de,(iy-state_size+STATE_REG_DE)
	ld.s hl,(iy-state_size+STATE_REG_HL)
	exx
	; Push the (remapped) GB AF to the stack
	ld de,(iy-state_size+STATE_REG_AF)
	ld h,flags_lut >> 8
	ld l,e
	ld.s e,(hl)
	push de
	 ; Get GB SP before we destroy IY
	 ld.s hl,(iy-state_size+STATE_REG_SP)
	 
	 ; Get the block cycle count (need to use this in case of a RAM block)
	 ld a,(recompile_struct+8+7)
	 ; Set the cycle count at block end relative to the current event,
	 ; which has been set to 1 cycle after block start
	 ld iy,0
	 dec a
	 ld iyl,a
	 ; Set the negative block cycle offset of the current instruction
	 cpl
	 ld (z80codebase+event_cycle_count),a
	 
	 ld bc,(CALL_STACK_DEPTH+1)*256
	; Pop GB AF into AF
	pop af
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
	
	; Calculate the current event cycle offset
	dec bc
	ld a,(z80codebase+event_cycle_count)
	ld c,a
	lea hl,iy
	add hl,bc
	ex de,hl
	
	; Save the frame-relative cycle count
	ld.sis hl,(frame_cycle_target)
	add.s hl,de
	jr c,_
	ld bc,CYCLES_PER_FRAME
	add hl,bc
_
	ld (ix-state_size+STATE_FRAME_COUNTER),hl
	
	; Save the serial cycle count
	ld.sis hl,(serial_cycle_count)
	or a
	sbc hl,de
	ld (ix-state_size+STATE_SERIAL_COUNTER),hl
	
	; Save the DIV cycle count
	ld.sis hl,(div_cycle_count)
	add hl,de
	ld (ix-state_size+STATE_DIV_COUNTER),hl
	
	; Save the actual value of TAC if the timer is running
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
	
	ld a,(z80codebase+intstate_smc)
	rra
	sbc a,a
	inc a
	ld (ix-state_size+STATE_INTERRUPTS),a
	
	ld a,(z80codebase+curr_rom_bank)
	ld (ix-state_size+STATE_ROM_BANK),a
	
	; Save the current MBC mode
	ld a,(cram_size)
	or a
	ld a,(mbc_rtc_last_latch)
	jr nz,_
	ld a,(z80codebase+mbc1_ram_smc)
	sub $28
	rlca
_
	and 1
	ld (ix-state_size+STATE_MBC_MODE),a
	
	; Save the currently mapped RAM bank
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
	
	; Save the actual audio port vales in the audio port space
	ld hl,z80codebase + audio_port_values
	lea de,ix-ioregs+NR10
	ld bc,audio_port_masks - audio_port_values
	ldir
	
	; Zero-fill the rest of the state
	lea hl,ix-state_size+STATE_END
	lea de,ix-state_size+STATE_END+1
	ld c,state_size-STATE_END-1
	ld (hl),b
	ldir
	
	; Copy into the state file
	ld l,c
	ld de,hram_saved
	ld b,$0200 >> 8
	ldir
	
ExitEmulationWithoutState:
	; Handle RTC saving
	ld a,(cram_size)
	or a
	jr z,++_
	call update_rtc
	ld ix,save_state_size_bytes - 44
	ld bc,(ix+44)
	add ix,bc
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
	ACALL(RestoreOriginalHardwareSettings)
	ei
	ld a,(exitReason)
	dec a
	srl a
	jr z,ExitDone
	jr c,_
	rra
	ccf
	jr c,ExitDone
	AJUMP(RestartFromHere)
_
	ld de,StateLoadedMessage
	ld a,(main_menu_selection)
	dec a
	jr z,_
	ACALL_SAFERET(SaveStateFiles)
	; Reindex the ROM in case a Garbage Collect occurred, and reinsert the RAM.
	ACALL_SAFERET(LoadROMAndRAMRestoreName)
	jr c,ExitDone
	ld de,StateSavedMessage
_
	or a
	sbc hl,hl
	ld a,(current_state)
	ld l,a
	push hl
	 ACALL(SetEmulatorMessage)
	pop hl
	ACALL(LoadStateFiles)
	jr nc,++_
	cp ERROR_NOT_ENOUGH_MEMORY
	jr nz,_
	ld hl,(save_state_size_bytes)
	ld a,l
	dec a
	or h
	jr z,++_
	ld a,ERROR_NOT_ENOUGH_MEMORY
_
	ACALL(DisplayError)
	scf
	jr z,ExitDone
	xor a
	ld (emulatorMessageText),a
_
	AJUMP(StartFromHere)
ExitDone:
	push af
	 xor a
	 ld (current_state),a
	 ACALL_SAFERET(SaveStateFiles)
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
	ld de,config_start
	ACALL(ConvertMemToFile)
	AJUMP(ArchiveWithWarning)
	
LoadROMAndRAMRestoreName:
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	dec hl
	dec hl
	dec hl
	ld (hl),a
	
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
	
LoadRAM:
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
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
	
	ld (cram_size),de
	inc de
	push de
	 inc de
	 inc de
	 inc de
	 inc de
	 inc de
	 ld hl,save_state_size_bytes
	 ACALL(InsertMemSafe)
	pop bc
	ret c
	ex de,hl
	; Set save state to uncompressed size 0 initially
	ld de,1
	ld (hl),de
	inc hl
	inc hl
	inc hl
	; Compression type is implicitly set to 0 here
	ld (hl),bc
	
LoadRAMAny:
	ld bc,(cram_size)
	ld a,b
	or c
	ld hl,mpZeroPage
	ld (cram_start),hl
	ret z
	ld hl,decompress_buffer
	ld (hl),l
	push hl
	pop de
	inc de
	dec bc
	ldir
	ld hl,ROMName
	ACALL(LookUpAppvar)
	jr c,LoadRAMNoVar
	ACALL(DecompressFile)

LoadRAMNoVar:
	ld hl,save_state_size_bytes
	ld de,(hl)
	add hl,de
	inc hl
	inc hl
	ld bc,(hl)
	inc hl
	inc hl
	inc hl
	ld (cram_start),hl
	dec bc
	ex de,hl
	ld hl,decompress_buffer
	ldir
	or a
	ret
	
	; Inputs: HL=compressed data
	;         BC=compressed size
	; Output: decompress_buffer contains decompressed data
	;         BC=decompressed size, or 0 on error
DecompressFile:
	ld a,b
	or c
	ret z
	; Only load compression type 0 (uncompressed)
	ld a,(hl)
	cpi
	ret po
	ld de,decompress_buffer
	or a
	jr z,DecompressUncompressed
	dec a
	jr nz,_
	push hl
	 call lzf_decompress
	pop hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ret z
_
	xor a
	ld b,a
	ld c,a
	ret
	
DecompressUncompressed:
	push bc
	 ldir
	pop bc
	ret
	
	; Input: HL=file in user RAM
	; Output: File potentially resized, but not moved
CompressFile:
	push hl
	 ld bc,(hl)
	 inc hl
	 inc hl
	 inc hl
	 dec bc
	 push bc
	  call lzf_compress
	  ex (sp),hl
	 pop bc
	 or a
	 sbc hl,bc
	 ex de,hl
	pop hl
	ret c
	push bc
	 inc bc
	 ld (hl),bc
	 inc hl
	 inc hl
	 ld (hl),1
	 inc hl
	 push hl
	  ACALL(DelMemSafe)
	 pop de
	pop bc
	ld hl,lzf_compress_buffer
	ldir
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
	inc hl
	ld (current_description),hl
	
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	ld (hl),'R'
	inc hl
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
	ld bc,256*3 - 1
	ld (hl),c
	ldir
	ld d,a
	ld e,a
LoadROMLoop:
	push de
	 ld hl,ROMName
	 ACALL(LookUpAppvar)
	pop de
	ld a,ERROR_FILE_MISSING
	ret c
	push hl
	 add hl,hl
	pop hl
	jr nc,LoadROMPageLoop
	call Arc_Unarc_Safe
	ld a,ERROR_NOT_ENOUGH_ARCHIVE
	ret c
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	dec hl
	dec hl
	dec hl
	ld (hl),a
	jr LoadROM

LoadROMPageLoop:
	ld a,d
	dec a
	cp (hl)
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
	xor a
_
	inc hl
	cp (hl)
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
	
GetStateRAMFileName:
	ld hl,ROMName
	push hl
	 xor a
_
	 inc hl
	 cp (hl)
	 jr nz,-_
	 dec hl
	 ld a,(current_state)
	 ld b,'v'
	 or a
	 jr nz,_
	 ld a,'V'
	 ld b,'A'
_
	 ld (hl),a
	 dec hl
	 ld (hl),b
	 dec hl
	 ld (hl),'S'
	pop hl
	ret
	
GetStateFileName:
	ld hl,ROMName
	push hl
_
	 inc hl
	 ld a,(hl)
	 or a
	 jr nz,-_
	 dec hl
	 ld a,(current_state)
	 or a
	 jr nz,_
	 ld a,'A'
_
	 ld (hl),a
	 dec hl
	 ld (hl),'t'
	 dec hl
	 ld (hl),'S'
	pop hl
	ret

SaveStateFiles:
	ld hl,save_state_size_bytes
	ld de,(hl)
	inc hl
	inc hl
	add hl,de
	push hl
	 ld bc,(hl)
	 ld a,c
	 dec a
	 or b
	 jr z,SaveRAMDeleteMem

	 ACALL(CompressFile)
	pop hl
	push hl
	 ld bc,(hl)
	 inc.s bc
	 inc bc
	 call checksum
	 ld (cart_ram_checksum),ix
	
	 ACALL(GetStateRAMFileName)
	 ACALL(LookUpAppvar) 
	 jr c,SaveRAMRecreate
	
	 ex de,hl
	 ex (sp),hl
	 push hl
	  dec de
	  dec de
	  inc bc
	  inc bc
	  call memcmp
	 pop hl
	 ex (sp),hl
	 jr z,SaveRAMDeleteMem
	
	 ; Delete the existing variable
	 push hl
	 pop ix
	 ld de,(ix-7)
	 ld d,(ix-4)
	 ld e,(ix-3)
	 call _DelVarArc
	
SaveRAMRecreate:
	pop de
	ld hl,ROMName
	ACALL(ConvertMemToFile)
	; Carry is set
	jr SaveState
	
SaveRAMDeleteMem:
	pop hl
	ACALL(DelMemSafeSizeBytes)
	or a
	
SaveState:
	push af
	 ACALL(GetStateFileName)
	 call _Mov9ToOP1
	 call _chkFindSym
	 call nc,_DelVarArc
	
	 ld a,(current_state)
	 or a
	 jr nz,_
	 ld hl,AutoSaveState
	 ld a,(hl)
	 res 1,(hl)
	 dec a
	 jr nz,SaveAutoStateDeleteMem
_
	 
	 ld hl,save_state_size_bytes
	 push hl
	  ACALL(CompressFile)
	 pop de
	 ld hl,ROMName
	 ACALL(ConvertMemToFile)
	 ACALL_SAFERET(ArchiveWithWarning)
	 jr ArchiveSaveRAM
	
SaveAutoStateDeleteMem:
	 ld hl,save_state_size_bytes
	 ACALL(DelMemSafeSizeBytes)
	 
ArchiveSaveRAM:
	pop af
	ret nc

	ACALL(GetStateRAMFileName)
ArchiveWithWarning:
	push hl
	 call _Mov9ToOP1
	 call Arc_Unarc_Safe	; Must be CALL due to special return address handling
	pop hl
	ret nc
	inc hl
	ld (errorArg),hl
	ld a,ERROR_NOT_ENOUGH_ARCHIVE
DisplayWarning:
	APTR(warning_text)
	jr _

	; A = error code
	; (errorArg) = error argument
	; Returns A=0 and Z flag set if ON was pressed
DisplayError:
	APTR(error_text)
_
	ld de,(current_buffer)
	push af
	 push de
	  ex (sp),hl
	  inc de
	  ld bc,160*240-1
	  ld (hl),BLUE_BYTE
	  ldir
	 pop hl
	 push bc
	   
	  ld a,5
	  ld (cursorRow),a
	  ld a,1
	  ld (cursorCol),a
	   
	  ld a,WHITE
	  ACALL(PutStringColor)
	   
	  APTR(error_messages)
	 pop bc
	pop de
	 
	xor a
_
	cpir
	dec d
	jr nz,-_
	
	ex de,hl
	ld hl,(errorArg)
	push hl
	 push de
	  ACALL(PutStringFormat)
	 pop hl
	pop hl
	call setup_menu_palette
	
	ACALL(SetCustomHardwareSettings)
	ACALL(WaitForKey)
	push af
	 ACALL(RestoreOriginalHardwareSettings)
	pop af
	ei
	ret
	
LoadStateInvalid:
	ld a,ERROR_FILE_INVALID
	scf
	ret
	
LoadStateFiles:
	ACALL(GetStateRAMFileName)
	inc hl
	ld (errorArg),hl
	ld ix,(cram_size)
	ld a,ixh
	or ixl
	jr z,_
	dec hl
	ACALL(LookUpAppvar)
	ld a,ERROR_FILE_MISSING
	ret c
	dec hl
	dec hl
	inc bc
	inc bc
	call checksum
_
	push ix
	 ACALL(GetStateFileName)
	 ACALL(LookUpAppvar)
	pop de
	ld a,ERROR_FILE_MISSING
	ret c
	
	push de
	 ACALL(DecompressFile)
	pop ix
	; Only load original Game Boy
	ld a,(decompress_buffer + (regs_saved + STATE_SYSTEM_TYPE - save_state_start))
	or a
	jr nz,LoadStateInvalid
	; Ensure state size is valid
	ld hl,save_state_size
	sbc hl,bc
	jr nz,LoadStateInvalid
	
	; Get the RAM file name (for error messages)
	push bc
	 ACALL(GetStateRAMFileName)
	pop bc
	; Validate the checksum
	ld hl,(decompress_buffer + (cart_ram_checksum - save_state_start))
	lea de,ix
	or a
	sbc hl,de
	jr nz,LoadStateInvalid
	
	; The state is valid, so load it
	inc bc
	push bc
	 ld hl,(save_state_size_bytes)
	 sbc hl,bc
	 ex de,hl
	 ld hl,save_state_start
	 push hl
	  jr nc,_
	  or a
	  sbc hl,hl
	  sbc hl,de
	  ex de,hl
	 pop hl
	 ACALL(InsertMemSafe)
	pop bc
	jr nc,++_
	ret
_
	  ACALL(DelMemSafe)
	 pop de
	pop bc
_
	ld (save_state_size_bytes),bc
	dec bc
	ld hl,decompress_buffer
	ldir
	
	; Load the RAM file (whose name we initialized above)
	AJUMP(LoadRAMAny)
	
	
LookUpAppvar:
	call _Mov9ToOP1
	call _chkFindSym
	ret c
GetDataSection:
	ex de,hl
	; Check if in RAM
	push hl
	 add hl,hl
	pop hl
	ld bc,9
	jr c,_
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
	
	
	; Populates and sorts the ROM list, and sets (romTotalCount)
ROMSearch:
	; Iterate through the VAT
	ld iy,romListStart
	ld hl,(progPtr)
	xor a
	ld c,a
ROMSearchLoop:
	; Check if we reached the VAT's end
	ex de,hl
	ld hl,(pTemp)
	sbc hl,de
	jr z,ROMSearchSort
	; Get the type byte
	ld a,(de)
	ld hl,-7
	add hl,de
	; Get the name size in D and the data pointer MSB in DEU
	ld de,(hl)
	xor appVarObj
	jr nz,ROMSearchNoMatch
	; Make sure the name is 5 characters long or less
	ld a,d
	cp 6
	jr nc,ROMSearchNoMatch
	push hl
	 push bc
	  ; Get the data pointer in DE
	  inc hl
	  inc hl
	  inc hl
	  ld d,(hl)
	  inc hl
	  ld e,(hl)
	  ; Advance to the variable data section
	  ACALL(GetDataSection)
	  ; Make sure the magic header is correct
	  ld de,MetaHeader
	  ld bc,8
	  call memcmp
	  jr z,ROMSearchMatch
	 pop bc
	pop hl
ROMSearchContinue:
	; Get the name length in D again and reset carry
	ld de,(hl)
	or a
ROMSearchNoMatch:
	; Subtract the name length to move to the next VAT entry
	ld e,1
	mlt de
	sbc hl,de
	jr ROMSearchLoop
	
ROMSearchMatch:
	  ; Get the description pointer in DE
	  ex de,hl
	  inc de
	 pop bc
	 ; Save the next node pointer
	 pea iy+3
	  ; Insert the new node into the max heap
	  ; Start with B=last node index and (SP) = last node pointer
	  ld b,c
	  push iy
ROMSearchHeapInsertLoop:
	   ; Move B to the parent node, or exit the loop if at the root
	   srl b
	   jr c,_
	   jr z,ROMSearchHeapInsertDone
	   dec b
_
	   push bc
	    ; Get the parent node pointer in IY and contents in IX,
	    ; and the description pointer in HL
	    call GetRomDescriptionByIndex
	    push de
	     call CompareDescriptions
	    pop de
	   pop bc
	   ; If the child is less than the parent, exit the loop
	   jr c,ROMSearchHeapInsertDone
	   ; Otherwise write the parent's contents to the child,
	   ; and save the parent node pointer in (SP)
	   ex (sp),iy
	   ld (iy),ix
	   jr ROMSearchHeapInsertLoop
ROMSearchHeapInsertDone:
	  pop hl
	 pop iy
	pop de
	; Write the new ROM entry to the current node pointer
	ld (hl),de
	ex de,hl
	; Increment the size
	inc c
	jr nz,ROMSearchContinue
	dec c
	
	; Finish out the heap sort
ROMSearchSort:
	ld a,c
	ld (romTotalCount),a
	or a
	ret z
ROMSearchHeapSortLoop:
	; Get the last node in the heap (or exit if it's the only node)
	dec a
	ret z
	ld b,a
	call GetRomDescriptionByIndex
	; B=0 after this call, which is the root node index
	; Update the heap size
	ld c,a
	; Hold the description pointer in DE
	ex de,hl
	; Save the old contents
	push ix
	 ; Put a pointer to the root node in (SP)
	 ld hl,romListStart
	 push hl
	  ; Move the root node contents (current max) to the former last node
	  ld ix,(hl)
	  ld (iy),ix
	  ; Balance the former last node contents starting at the root
	  scf
ROMSearchHeapBalanceLoop:
	  ; Carry must be set here, move to the left child node index
	  rl b
	  ; If this node doesn't exist, we're done balancing
	  ld a,b
	  sub c
	  jr nc,ROMSearchHeapBalanceDone
	  push bc
	   ; Get the left node pointer in IY and contents in IX,
	   ; and the description pointer in HL
	   call GetRomDescriptionByIndex
	   ; If the right node doesn't exist, balance to the left
	   inc a
	   jr z,ROMSearchHeapBalanceLeft
	   ; Check whether the left or right node should be selected for balancing
	   push de
	    ; Put the left node's description in DE
	    ex de,hl
	    ; Move IY to the right node and get its contents in IX
	    ; and its description pointer in HL
	    lea iy,iy+3
	    call GetRomDescription
	    push hl
	     push de
	      call CompareDescriptions
	     pop de
	    pop hl
	    ; If the left node is less than the right node, balance to the right
	    jr c,ROMSearchHeapBalanceRight
	    ; Otherwise, restore IY, IX, and HL to the left node
	    ex de,hl
	    lea iy,iy-3
	    ld ix,(iy)
	   pop de
	   jr ROMSearchHeapBalanceLeft
ROMSearchHeapBalanceRight:
	   pop de
	   ; Move the node index to the right node
	  pop bc
	  inc b
	  push bc
ROMSearchHeapBalanceLeft:
	   push de
	    call CompareDescriptions
	   pop de
	  pop bc
	  ; If the parent node is greater than the child node, we're done balancing
	  jr nc,ROMSearchHeapBalanceDone
	  ; Otherwise write the child's contents to the parent,
	  ; and save the child node pointer in (SP)
	  ex (sp),iy
	  ld (iy),ix
	  ; Carry is set
	  jr ROMSearchHeapBalanceLoop
	  
ROMSearchHeapBalanceDone:
	 pop hl
	pop de
	; Write the former last node contents to the current node
	ld (hl),de
	ld a,c
	jr ROMSearchHeapSortLoop
	
	  
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
	
BackupOriginalHardwareSettings:
	ld ix,originalHardwareSettings
BackupHardwareSettings:
	ld hl,(mpIntEnable)
	ld (ix+0),hl
	ld hl,(mpIntLatch)
	ld (ix+3),hl
	ld a,(mpFlashWaitStates)
	ld (ix+6),a
	ld a,mb
	ld (ix+7),a
	ld a,(mpKeypadScanMode)
	ld (ix+8),a
	ld a,(mpLcdImsc)
	ld (ix+9),a
	ret
	
SetCustomHardwareSettings:
	di
	ld a,$FB      ; EI
	ld (z80codebase + wait_for_interrupt_stub),a
	ld hl,$C94976 ; HALT \ RET.LIS
	ld (z80codebase + wait_for_interrupt_stub + 1),hl
	APTR(customHardwareSettings)
	push hl
	pop ix
	jr RestoreHardwareSettings
	
RestoreOriginalHardwareSettings:
	ld iy,flags
	ld ix,originalHardwareSettings
RestoreHardwareSettings:
	ld hl,(ix+0)
	ld (mpIntEnable),hl
	ld hl,(ix+3)
	ld (mpIntLatch),hl
	ld a,(ix+6)
	ld (mpFlashWaitStates),a
	ld a,(ix+7)
	ld mb,a
	ld a,(ix+8)
	ld (mpKeypadScanMode),a
	ld a,(ix+9)
	ld (mpLcdImsc),a
	ret
	
RestoreOriginalLcdSettings:
	ld hl,vRam
	ld de,originalLcdSettings
	
	; In: (DE) = timing (12 bytes)
	;     (DE+12) = LCD control (3 bytes)
	;     (DE+15) = left side of window (1 byte)
	;     (DE+16) = right side of window (2 bytes)
	;     (DE+18) = top side of window (1 byte)
	;     (DE+19) = bottom side of window (1 byte)
	;     (DE+20) = number of frames to wait (plus 1)
	;     HL = new LCD base address
SetLcdSettings:
	push hl
	 push de
	 pop ix
	 ld hl,(currentLcdSettings)
	 or a
	 sbc hl,de
	 jr z,SetLcdSettingsFast
	 ld (currentLcdSettings),de
	 ; Wait for the the number of specified frames
	 ld b,(ix+20)
_
	 ld hl,mpLcdIcr
	 ld (hl),4
	 ld l,mpLcdRis & $FF
_
	 bit 2,(hl)
	 jr z,-_
	 djnz --_
	 ; Turn off LCD
	 ld l,mpLcdCtrl & $FF
	 res 0,(hl)
	 
	 ; Start SPI transfer
	 ld hl,mpSpiTransfer
	 ld (hl),1
	 ld c,$2A
	 ; Set left/right window bounds
	 call spiCmd
	 ; Left MSB=0
	 call spiParam
	 ld a,(ix+15) ; Left LSB
	 call spiParam
	 ld de,(ix+16)
	 ld a,d ; Right MSB
	 call spiParam
	 ld a,e ; Right LSB
	 call spiParam
	 
	 ; Set top/bottom window bounds
	 call spiCmd
	 ; Top MSB=0
	 call spiParam
	 ld de,(ix+18)
	 ld a,e ; Top LSB
	 call spiParam
	 call spiParam
	 ld a,d ; Bottom LSB
	 call spiParam
	 
	 ; Set data mode
	 call spiCmd
	 ; End transfer
	 ld l,mpSpiTransfer & $FF
	 ld (hl),a
	
	 ; Set timing parameters
	 lea hl,ix+0
	 ld de,mpLcdTiming0
	 ld bc,12
	 ldir
SetLcdSettingsFast:
	pop hl
	; Set LCD base
	ld (mpLcdBase),hl
	; Set LCD control (turning LCD back on)
	ld hl,(ix+12)
	ld (mpLcdCtrl),hl
	ret
	
Set8BitWindow:
	ld hl,(current_buffer)
	ACALL(Set4BitWindowAny)
	APTR(lcdSettings8Bit)
	ex de,hl
	ld hl,(current_display)
	jr SetLcdSettingsTrampoline
	
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
	ld b,BG_COLOR_0
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
	ld hl,(current_display)
Set4BitWindowAny:
	push hl
	 APTR(lcdSettings4Bit)
	 ex de,hl
	pop hl
SetLcdSettingsTrampoline:
	AJUMP(SetLcdSettings)
	
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
	ld hl,(current_buffer)
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
	 ; Make sure decompressed size is 160*240
	 inc hl
	 ld a,(hl)
	 sub (160*240) >> 8
	 dec hl
	 or (hl)
	 jr nz,bad_skin_file
	pop de
	call lzf_decompress
	
	ACALL(Set8BitWindow)
	
	xor a
bad_skin_file:
	ld hl,palette_backup
	ld de,mpLcdPalette
	ld bc,32
	ldir
	ret z
	
	pop hl
	push hl
no_skin:
	pop de
	inc de
	ld (hl),BLACK_BYTE
	ld bc,160*240-1
	ldir
	AJUMP(Set8BitWindow)
	
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
	
customHardwareSettings:
	;mpIntEnable
	.dl $000801
	;mpIntLatch
	.dl $000011
	;mpFlashWaitStates
	.db 2
	;MBASE
	.db z80codebase >> 16
	;mpKeypadScanMode
	.db 3
	;mpLcdImsc
	.db 4
	
lcdSettings4Bit:
	; LcdTiming0
	.db $38,$03,$9C,$1F
	; LcdTiming1
	.db $3F,$09,$89,$04
	; LcdTiming2
	.db $00,$78,$EF,$00
	; LcdCtrl
	.dl $010C25
	; Window left
	.db 0
	; Window right
	.dw 319
	; Window top
	.db 0
	; Window bottom
	.db 239
	; Number of frames to wait
	.db 1
	
lcdSettings8Bit:
	; LcdTiming0
	.db $44,$03,$04,$0D
	; LcdTiming1
	.db $4F,$08,$F0,$00
	; LcdTiming2
	.db $02,$78,$1F,$01
	; LcdCtrl
	.dl $010C27
	; Window left
	.db 80
	; Window right
	.dw 239
	; Window top
	.db 48
	; Window bottom
	.db 191
	; Number of frames to wait
	.db 2
	
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
	.db 0,0,$7E,$FF,0,$00,$00,$F8,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$E1
	.db $80,$BF,$F3,$FF,$BF,$FF,$3F,$00,$FF,$BF,$7F,$FF,$9F,$FF,$BF,$FF
	.db $FF,$00,$00,$BF,$77,$F3,$F1,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.db $91,0,$00,$00,0,$00,$FF,$FC,$FF,$FF,$00,$00,$FF
hmem_init_size = $ - hmem_init
	
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
warning_text:
	.db "Warning: ",0
error_text:
	.db "Error: "
error_messages:
	.db 0
	DEFINE_ERROR("ERROR_FILE_MISSING", "Missing AppVar %s")
	DEFINE_ERROR("ERROR_FILE_INVALID", "Invalid AppVar %s")
	DEFINE_ERROR("ERROR_NOT_ENOUGH_ARCHIVE", "Failed to archive AppVar\n %s")
	DEFINE_ERROR("ERROR_UNSUPPORTED_MBC", "Unsupported cartridge type %02X")
	DEFINE_ERROR("ERROR_INVALID_ROM", "ROM is invalid")
	DEFINE_ERROR("ERROR_NOT_ENOUGH_MEMORY", "Need %d more bytes free RAM")
	DEFINE_ERROR("ERROR_RUNTIME", "Encountered a runtime error!")
	DEFINE_ERROR("ERROR_INVALID_OPCODE", "Ran an invalid Game Boy opcode\n at %06X")
	
ErrorNoROMsFound:
	.db "No ROMs found!",0
	
StateLoadedMessage:
	.db "State %c loaded",0
	
AutoStateLoadedMessage:
	.db "Auto state loaded",0
	
StateSavedMessage:
	.db "State %c saved",0
	