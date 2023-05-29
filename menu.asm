#define ITEM_LINK 0
#define ITEM_CMD 1
#define ITEM_DIGIT 2
#define ITEM_OPTION 3
#define ITEM_KEY 4
#define ITEM_ROM 5

#define ITEM_ROMONLY $80

#define ROMS_PER_PAGE 15

#define UNMAPPED_KEY 41

ApplyConfiguration:
	; Copy the global config into the active config
	ld hl,(global_config_start)
	ld de,OptionConfig-1
	push de
	 ld bc,config_size-1
	 ldir
	pop de
	; If no ROM is loaded, our work here is done
	ld a,(ROMName+1)
	or a
	ret z

	; Display next frame always
	ld a,$4F ;LD R,A
	ld (z80codebase+updateSTAT_enable_catchup_smc),a
	ld (z80codebase+updateSTAT_full_enable_catchup_smc),a
	ld (z80codebase+ppu_mode0_enable_catchup_smc),a
	ld (z80codebase+ppu_mode2_enable_catchup_smc),a
	ld (z80codebase+ppu_lyc_enable_catchup_smc),a

	; Override global config with game-specific config
	ld hl,(game_config_start)
	ld b,(hl) ;option_config_count
option_config_loop:
	inc hl
	inc de
	ld a,(hl)
	cp -1
	jr z,_
	ld (de),a
_
	djnz option_config_loop

	; Check for conflicting keys between configs
	; C=0
	inc hl
	inc hl
key_conflict_retry:
	ld de,KeyConfig
	ld b,key_config_count
	push hl
key_conflict_loop:
	 ld a,(hl)
	 cp -1
	 jr nz,++_
	 ; This key is inherited from global, so make sure it has no conflict
	 ; among game-specific keys
	 ld a,(de)
	 cp UNMAPPED_KEY
	 jr z,++_
	 ex (sp),hl
	 push hl
	  push bc
	   ld bc,key_config_count
	   cpir
	  pop bc
	  jr nz,_
	  ; If a conflict was found, reset it to global and start over
	  dec hl
	  ld c,$FF
	  ld (hl),c
	 pop hl
	pop de
	jr key_conflict_retry
_
	 pop hl
	 ex (sp),hl
_
	 inc hl
	 inc de
	 djnz key_conflict_loop
	pop af
	; Record whether any conflicts were removed
	push bc

	 ; Now override global config with game-specific config
	 ld b,key_config_count
key_config_loop:
	 dec hl
	 dec de
	 ld a,(hl)
	 cp -1
	 jr z,_
	 ld (de),a
_
	 djnz key_config_loop

	 ; Key SMC configuration
	 APTR(KeySMCList)
	 ex de,hl
	 ld hl,key_smc_turbo
	 ld ix,OptionConfig-key_config_count
	 ld b,key_config_count
key_config_smc_loop:
	 ld a,(ix+key_config_count+KeyConfigOffset)
	 inc ix
	 dec a
	 ld c,a
	 cpl
	 and %00111000
	 rrca
	 rrca
	 ld (hl),a
	 inc hl
	 ld a,c
	 and %00000111
	 add a,a
	 add a,a
	 add a,a
	 add a,$46
	 ld (hl),a
	 ld a,(de)
	 inc de
	 add a,l
	 jr nc,_
	 ld l,$FF
	 inc hl
_
	 ld l,a
	 djnz key_config_smc_loop

	 ; IX = OptionConfig
	 ; Frameskip value
	 ld a,(ix+FrameskipValueOffset)
	 ld (speed_display_smc_0),a
	 inc a
	 ld (frameskip_value_smc),a
	 ld (skippable_frames),a

	 ; Frameskip type
	 ld a,(ix+FrameskipTypeOffset)
	 ld c,a
	 sub 2
	 jr nz,_
	 ld (speed_display_smc_0),a
_
	 and $10
	 add a,$18
	 ld (frameskip_type_smc),a
	 ld a,c
	 or a
	 jr z,_
	 ld a,no_frameskip - (frameskip_type_smc+2)
_
	 ld (frameskip_type_smc+1),a

	 ; Speed display
	 ld a,(ix+SpeedDisplayOffset)
	 ld c,a
	 sub 1
	 sbc a,a
	 and NoSpeedDisplay - YesSpeedDisplay
	 add a,YesSpeedDisplay - (speed_display_smc_1 + 1)
	 ld (speed_display_smc_1),a
	 ld a,c
	 sub 2
	 and $10
	 add a,$CE
	 ld (speed_display_smc_3),a
	 ld a,c
	 sub 3
	 and $10
	 add a,$18
	 ld (speed_display_smc_2),a

	 ; Auto Save State
	 ld a,(ix+AutoSaveStateOffset)
	 ld (should_auto_save),a
	
	 ; Time zone
	 ld bc,0
	 ld c,(ix+TimeZoneOffset)
	 APTR(TimeZoneOffsetTable)
	 add hl,bc
	 ld e,(hl)
	 ld d,225
	 mlt de
	 sbc hl,hl
	 ; Daylight saving time
	 ld a,(ix+DaylightSavingTimeOffset)
	 or a
	 jr z,_
	 ld hl,60*60
_
	 sbc hl,de
	 ld a,c
	 cp 19
	 jr nc,_
	 add hl,de
	 add hl,de
_
	 ld (timeZoneOffsetSeconds),hl

	 ; Turbo toggle
	 ld a,(ix+TurboModeOffset)
	 dec a
	 and turbo_skip_toggle - (turbo_toggle_smc+1)
	 ld (turbo_toggle_smc),a
	 ld a,(turbo_active)
	 add a,a
	 add a,a
	 add a,a
	 add a,$20
	 ld (turbo_keypress_smc),a

	 ; Scaling mode
	 ld a,(ix+ScalingModeOffset)
	 ld (active_scaling_mode),a
	 ld l,a

	 ; Scaling method
	 ld a,(ix+ScalingMethodOffset)
	 ld (active_scaling_method),a
	 and l
	 ld hl,do_frame_flip
	 jr z,_
	 ld hl,do_frame_flip_gram_swap
_
	 ld (do_frame_flip_gram_swap_smc),hl

	 ; Scale tracking
	 ld a,(ix+ScaleTrackingOffset)
	 ld (active_scale_tracking),a

	 ; Palette selection
	 ld a,(ix+PaletteSelectionOffset)
	 ld hl,default_palette
	 or a
	 jr z,_
	 APTR(ManualPaletteIndexTable-1)
	 ld c,a
	 add hl,bc
_
	 ld a,(currentSystemType)
	 or a
	 jr nz,_
	 ; Disable color adjustment for classic palette
	 ld a,(hl)
	 sub $1D
	 jr z,++_
_
	 ; Adjust colors
	 ld a,(ix+AdjustColorsOffset)
_
	 push af
	  ; Load palettes and setup color adjustment tables
	  ld c,(hl)
	  ACALL(LoadPalettes)
	  ; BC=0
	 pop bc
	 ; Set gamma setting based on color adjustment
	 ACALL(SetupGamma)
	pop af
	ret nc
	ld a,ERROR_KEY_CONFLICT
	AJUMP(DisplayWarning)

RefreshRomListFrame:
	ld a,(romListFrameStart)
	ld hl,(romTotalCount)
	ld h,ROMS_PER_PAGE
	or a
	jr z,+++_
	sub l
	jr c,++_
_
	sub h
	jr nc,-_
_
	add a,l
_
	ld (romListFrameStart),a
	sub l
	neg
	cp h
	jr c,_
	ld a,h
_
	inc a
	ld (romListFrameCount),a
	ld hl,main_menu_selection + LoadGameMenuIndex
	cp (hl)
	ret nc
	ld (hl),a
	ret
	
ClearMenuBuffer:
	ld hl,menu_frame_buffer
	ld (current_buffer),hl
	MEMSET_FAST_TAIL(menu_frame_buffer, 320*240, BLUE)
	
	; Input: C = palette index, A = color adjust mode
	; Output: BC = 0
	; Destroys: AF,DE,HL,IX
LoadPalettes:
	ld d,$C9 ;RET
	or a
	jr z,LoadPalettesNoColorAdjust
	ld d,a
	ld e,40
	mlt de
	ld hl,GreenAdjustOffsetsGBC-40
	add hl,de
	ex de,hl
	ld ix,(ArcBase)
	add ix,de
	ld de,adjust_color_lut
LoadPaletteColorAdjustRowLoop:
	ld a,e
	and 3
	jr nz,LoadPaletteColorAdjustNextByte
	ld l,(ix)
	inc ix
	ld h,a
	add hl,hl
LoadPaletteColorAdjustNextByte:
	ld b,(ix)
	inc ix
LoadPaletteColorAdjustNextBit:
	; Assign new value of low 3 green bits (mask $8C)
	xor e
	and $8C
	xor e
	ld e,a
	; Conditionally decrement green offset
	sla b
	ld a,l
	jr nc,_
	sub $20
	ld l,a
	jr nc,_
	dec h
_
	; Write green offset to LUT
	dec d ;adjust_green_lsb_lut
	ld (de),a
	ld a,h
	inc d
	inc d ;adjust_green_msb_lut
	ld (de),a
	; Fill low byte conversion LUT as well
	ld a,e
	sra a \ sra a \ sra a
	and $8C
	dec d ;adjust_color_lut
	ld (de),a
	; Increment low 3 green bits (mask $8C)
	ld a,e
	or d ;$73
	inc a
	jr nz,LoadPaletteColorAdjustNextBit
	; Increment upper 2 green bits and 3 blue bits (mask $73)
	; This works because the $8C bits are already set in E
	inc e
	jr nz,LoadPaletteColorAdjustRowLoop
	ld d,$21 ;LD HL,
LoadPalettesNoColorAdjust:
	ld a,d
	ld (adjust_color_enable_smc),a
	ld a,c

	ld ix,(ArcBase)
	ld bc,PaletteIndex
	add ix,bc
	ld e,a
	and $1F
	ld c,a
	ld b,3
	mlt bc
	add ix,bc
	
	ld a,(currentSystemType)
	or a
	ld a,d
	rla
	ld a,e
	ld hl,update_palettes_gbc_smc
	ld de,prepare_next_frame_gbc_smc+1
	jr z,LoadPalettesGB
	push hl
	 ld a,$18 ;JR
	 ld (hl),a
	 inc hl
	 ld (hl),update_palettes_gbc - (update_palettes_gbc_smc+2)
	 ld hl,mpLcdPalette + (SCREEN_OFF_COLOR*2)
	 ld (hl),$FF
	 inc hl
	 ld (hl),$FF
	 ex de,hl
	 ld (hl),prepare_next_frame_gbc_no_adjust - (prepare_next_frame_gbc_smc+2)
	 jr c,_
	 ld (hl),prepare_next_frame_gbc_adjust - (prepare_next_frame_gbc_smc+2)
_
	 dec hl
	 ld (hl),a
	 jp (hl)
	
LoadPalettesGB:
	ld (hl),$21 ;LD HL,
	ex de,hl
	ld (hl),BGP_max_value & $FF
	dec hl
	ld (hl),$3A ;LD A,(BGP_max_value)
	
	; Load BGP
	ld de,overlapped_bg_palette_colors
	ld c,(ix+2)
	ACALL(LoadSinglePalette)
	ex de,hl
	ld l,bg_palette_colors & $FF
	ld de,mpLcdPalette + (BG_COLOR_0 * 2)
	ld c,4*2
	ldir
	
	; Load OBP0
	ld c,(ix)
	bit 5,a
	jr nz,_
	ld c,(ix+2)
_
	ACALL(LoadSinglePalette)
	
	; Load OBP1
	ld c,(ix+1)
	rlca
	jr c,_
	ld c,(ix)
	rlca
	jr c,_
	ld c,(ix+2)
_
	; Input:  BC = palette offset, DE = output table ptr
	; Output: DE = next output table ptr, BC = 0
LoadSinglePalette:
	push af
	 push ix
	  ld hl,(ArcBase)
	  add hl,bc
	  ld bc,PaletteDictionary
	  add hl,bc
	  ld ix,overlapped_palette_index_lut
load_single_palette_input_loop:
	  push hl
	   ld bc,(hl)
	   call adjust_color
	   or a
load_single_palette_output_loop:
	   sbc hl,hl
	   ld l,(ix)
	   add hl,hl
	   add hl,de
	   ld (hl),c
	   inc hl
	   ld (hl),b
	   ld a,ixl
	   add a,16
	   ld ixl,a
	   jr nc,load_single_palette_output_loop
	  pop hl
	  inc hl
	  inc hl
	  ld bc,4
	  adc a,c
	  ld ixl,a
	  and 3
	  jr nz,load_single_palette_input_loop
	  ld hl,64*2
	  add hl,de
	  ex de,hl
	  ldir
	 pop ix
	pop af
	ret
	
SetupOriginalGamma:
	ld b,3
SetupGamma:
	ld c,spiGammaConfigSize
	mlt bc
	ld hl,spiGammaUniform
	add hl,bc
	SPI_TRANSFER_CMDS(spiGammaCommandDescriptors)
	ex de,hl
	ld de,mpLcdPalette + (BLUE * 2)
	ld c,menuPaletteSize
	ldir
	; Write black and white to the palette
	ex de,hl
	ld (hl),c
	inc hl
	dec bc
	ld (hl),bc
	inc (hl)
	ret
	
BackupAndShowConfirmStateOperation:
	ACALL(BackupMiniScreen)
ShowConfirmStateOperation:
	; Just return NZ if the state does not exist
	call check_valid_state
	dec a
	ret m
	ld de,ConfirmLoadState
	ld hl,(main_menu_selection)
ShowConfirmRestartOperation:
	; Return NZ if confirmation is disabled for this type
	ld a,(ConfirmStateOperation)
	and l
	dec a
	ret m
	jr z,_
	ld de,ConfirmSaveState
_
	push de
	 ACALL(SetMenuWindow)
	pop de
	ld hl,(current_state)
ShowConfirmationDialog:
	push hl
	 ld hl,(ArcBase)
	 add hl,de
	 push hl
	  ACALL(ClearMenuBuffer)
	  ld a,WHITE
	  ld hl,1<<8|5
	  ACALL(PutStringFormatColorXY)
	 pop hl
	pop hl
	
	ld b,2
	ld c,b
ConfirmationDialogDisplayLoop:
	ld hl,1<<8|20
	ld (cursorRowCol),hl
	APTR(ConfirmText)
	push bc
_
	 ld a,WHITE
	 ld e,'>'-' '
	 djnz _
	 ld a,OLIVE
	 ld e,b ;' '-' '
_
	 call SetStringColor
	 ld a,e
	 push bc
	  push hl
	   call PutCharTranslated
	  pop hl
	  ACALL(PutString)
	 pop bc
	 dec c
	 jr nz,--_
ConfirmationDialogKeyLoop:
	 ACALL(WaitForKey)
	pop bc
	ret z
	cp 1
	jr nz,_
	ld b,a
	jr ConfirmationDialogDisplayLoop
_
	cp 4
	jr nz,_
	ld b,2
	jr ConfirmationDialogDisplayLoop
_
	cp 15
	jr z,_
	push bc
	 cp 9
	 jr z,++_
	 jr ConfirmationDialogKeyLoop
_
	ld b,2
	push bc
_
	 ACALL(GetKeyCode)
	 or a
	 jr nz,-_
	pop bc
	dec b
	dec b
	ret
	
ItemSelectCmd:
	ld hl,CmdList
	ld c,a
	ld b,2
	mlt bc
	add hl,bc
	ld bc,(ArcBase)
	add hl,bc
	ld hl,(hl)
	dec.s hl
	add hl,bc
	jp (hl)
	
ItemSelectRom:
	APTR(CmdExit)
	push hl
GetSelectedROMName:
	ld l,a
	ld h,3
	mlt hl
	ld de,romListStart
	add hl,de
	ld hl,(hl)
	ld bc,(hl)
	ld de,ROMNameToLoad+1
	push hl
	 push de
_
	  ld a,(hl)
	  ld (de),a
	  dec hl
	  inc de
	  djnz -_
	  xor a
	  ld (de),a
	 pop de
	pop ix
	inc a
	jp GetRomDescriptionFromVAT
	
ItemSelectKey:
	ACALL(GetOption)
	push bc
	 ld a,(bc)
	 push af
	  xor a
	  ld (bc),a
	  ACALL(draw_current_menu)
	  ; Direct key polling with no translation layer
_
	  call poll_single_key
	  jr nz,-_
_
	  call poll_single_key
	  jr z,-_
	 pop de
	 or a
	 jr nz,_
	 ld a,d
_
	 ld e,a
	 ld a,KeyConfigOffset
	 ACALL(GetOption)
	 push bc
	 pop hl
	 ld bc,key_config_count
	 ld a,e
	 cpir
	 jr nz,_
	 ld a,d
	 cp UNMAPPED_KEY
	 jr z,_
	 dec hl
	 ld (hl),a
	 ld a,e
_
	pop hl
	ld (hl),a
	ACALL(draw_current_menu)
	jr menu_loop

CmdRestart:
	; Use the Load State confirmation setting
	ld l,1
	ld de,ConfirmRestartGame
	ACALL(ShowConfirmRestartOperation)
	ld a,RestartExitReason
	jr _
	
ItemSelectDigit:
	cp 2
	jr z,menu_loop
	ACALL(ShowConfirmStateOperation)
	ld a,4
_
	jr nz,CmdExit
	xor a
	jr ItemSelectLink

emulator_menu_ingame:
	ld a,(currentSystemType)
	or a
	call z,convert_palette_for_menu
	xor a
emulator_menu:
	push af
	 ACALL(BackupMiniScreen)
	 .db $3E ;LD A,
emulator_menu_no_backup:
	push af
	 ACALL(SetMenuWindow)
	 ; If the state slot was changed, verify load state is valid
	 ld hl,main_menu_selection
	 ld b,(hl)
	 djnz _
	 call check_valid_state
	 jr nz,_
	 inc (hl)
_
	pop af
	
ItemSelectLink:
	ld (current_menu),a
	or a
	sbc hl,hl
	ld l,a
	ld de,main_menu_selection
	add hl,de
	ld (current_menu_selection),hl
	dec a
	jr nz,_
	ACALL(ROMSearch)
_
	ACALL(redraw_current_menu)
ItemSelectOption:
	
menu_loop:
	ACALL(WaitForKey)
	jr nz,_
	ld a,2
	jr CmdExit
_
	call get_current_menu_selection
	dec a
	jr z,menu_down
	cp 3
	jr c,menu_left_right
	jr z,menu_up
	cp 9-1
	jr z,menu_select
	cp 56-1
	jr z,menu_delete
	cp 15-1
	jr nz,menu_loop
	
	ld hl,current_menu
	xor a
	cp (hl)
	jr nz,ItemSelectLink
	
CmdExit:
	ld (exitReason),a
	ex af,af'
_
	ACALL(GetKeyCode)
	or a
	jr nz,-_
	ret
	
menu_up:
	ld a,(menuPrevItem)
	ld (bc),a
	ACALL(draw_current_menu)
menu_loop_trampoline:
	jr menu_loop
	
menu_down:
	ld a,(menuNextItem)
	ld (bc),a
	ACALL(draw_current_menu)
	jr menu_loop_trampoline
	
menu_left_right:
	dec a
	add a,a
	dec a
	ld de,ItemChangeCallbacks
_
	ACALL(DoCurrentItemCallback)
	jr menu_loop_trampoline
	
menu_delete:
	ld de,ItemDeleteCallbacks
	jr -_
	
menu_select:
	ld de,ItemSelectCallbacks
DoCurrentItemCallback:
	ld hl,(current_item_ptr)
DoItemCallback:
	ld c,(hl)
	res 7,c
	inc hl
	ex de,hl
	ld b,2
	mlt bc
	add hl,bc
	ld bc,(ArcBase)
	add hl,bc
	ld hl,(hl)
	dec.s hl
	add hl,bc
	ld b,a
	ld c,0
	ld a,(de)
	jp (hl)
	
ItemDeleteRom:
	ACALL(GetSelectedROMName)
	; Don't allow deleting the currently loaded ROM
	ld bc,(current_description)
	sbc hl,bc
	ret z
	ex de,hl
	ld de,ConfirmDeleteROM
ItemDeleteStateFinish:
	ACALL(ShowConfirmationDialog)
	jr z,redraw_current_menu_trampoline
	pop de
	ld a,5
	jr CmdExit
	
ItemDeleteState:
	call check_valid_state
	ret z
	ld de,ConfirmDeleteState
	ld hl,(current_state)
	jr ItemDeleteStateFinish
	
ItemChangeRom:
	ld hl,romListFrameStart
	ld a,(hl)
	djnz _
	add a,ROMS_PER_PAGE
	jr ++_
_
	sub ROMS_PER_PAGE
	jr c,redraw_current_menu
_
	ld (hl),a
redraw_current_menu_trampoline:
	jr redraw_current_menu

ItemChangeKey:
	sub RightKeyOffset
	cp MenuKeyOffset-RightKeyOffset
	ret nc
	xor 4
	inc a
	ld (main_menu_selection + ControlsMenuIndex),a
	jr draw_current_menu_trampoline

ItemChangeDigit:
	ld e,a
	ld a,b
	and 9
	ld d,a
	ld hl,current_state
	ld a,e
	sub 2
	jr nz,_
	ACALL(GetOption)
	push bc
	pop hl
	ld (hl),a
_
	rrd
	add a,d
	daa
	rld
	ld a,e
	or a
	call z,check_valid_state
	jr z,-_
	jr draw_current_menu_trampoline

ItemDeleteDigit:
	sub 2
	jr nz,ItemDeleteState
ItemDeleteKey:
ItemDeleteOption:
	ld d,a
	ACALL(GetOption)
	jr nz,ItemRevertGameSpecific
	ld a,d
	sub KeyConfigOffset
	ret c
	; Don't allow unmapping in-game buttons or menu
	dec a
	cp UndeletableKeysEndOffset-UndeletableKeysStartOffset
	ret c
	ld a,(bc)
	cp UNMAPPED_KEY
	ret z
	ld a,UNMAPPED_KEY
	jr ItemDeleteKeyUnmapFinish
ItemRevertGameSpecific:
	ld a,-1
ItemDeleteKeyUnmapFinish:
	ld (bc),a
draw_current_menu_trampoline:
	jr draw_current_menu
	
ItemChangeOption:
	ld d,b
	ACALL(GetOption)
	add a,d
	add a,(hl)
_
	sub (hl)
	jr nc,-_
	add a,(hl)
	ld (bc),a
	jr draw_current_menu
	
redraw_current_menu:
	ACALL(RefreshRomListFrame)

	; Apply configuration (updates palette and key settings)
	ACALL(ApplyConfiguration)
	ACALL(ClearMenuBuffer)

	; Skip description display if on ROM list
	ld hl,(current_menu)
	dec l
	jr z,draw_current_menu
	
	; Display only description if no ROM is loaded
	ld a,(ROMName+1)
	or a
	ld c,35
	jr z,draw_current_description
	
	; Draw mini screen if on main menu
	inc l
	jr nz,_
	ACALL(draw_mini_screen)
_
	
	; Draw ROM internal name and checksum
	ld ix,(rom_start)
	ld bc,$0134
	add ix,bc
	ld b,(ix+$014E-$0134)
	ld c,(ix+$014F-$0134)
	push bc
	 push ix
	  APTR(TitleChecksumFormat)
	  push hl
	   ld a,MAGENTA
	   ld de,1<<8|40
	   ACALL(PutStringFormatColorXYIgnoreInvalid)
	  pop hl
	 pop hl
	pop hl
	
	ld c,30
draw_current_description:
	ld b,1
	ld hl,(current_description)
	ld a,MAGENTA
	ACALL(PutNStringColorXY)
	
draw_current_menu:
	; Erase the help text
	MEMSET_FAST(menu_frame_buffer+(320*205), 320*30, BLUE)
	
	call get_current_menu_selection
	ld a,(bc)
	ld c,a
	push af
	
	 ; HL = menu structure, C = highlighted item index
draw_menu:
	 ld b,(hl)
	 inc hl
	 inc b
	 push bc
	  ld a,WHITE
	  call SetStringColor
	  ex de,hl
	  ld hl,$FF00
	  ld c,l
	  push hl
	   jr draw_menu_title
draw_menu_loop:
	 dec c
	 push bc
	  jr nz,_
	  push de
	   ld a,MAGENTA
	   ld bc,1<<8|205
	   ACALL(PutStringColorXY)
	   ld (current_item_ptr),hl
	   ld a,WHITE
	   jr draw_menu_item
_
	  xor a
	  cpir
	  ld a,(hl)
	  add a,a
	  jr nc,_
	  cp ITEM_DIGIT<<1
	  ld a,(ROMName+1)
	  call z,check_valid_load_state
	  or a
	  ld a,GRAY
	  jr z,draw_menu_item_push
_
	 pop bc
	 push bc
	  ; Update the "next item" offset
	  ld a,c
	  cp e
	  jr c,_
	  ld e,a
_
	  ; Update the "prev item" (minus 1) offset
	  dec a
	  cp d
	  jr nc,_
	  ld d,a
_
	  ld a,OLIVE
draw_menu_item_push:
	  push de
draw_menu_item:
	   call SetStringColor

	   ld de,ItemDisplayCallbacks
	   ACALL(DoItemCallback)
	   inc de
	   
draw_menu_title:
	   push hl
	    ex de,hl
	    ld a,c
	    ld c,(hl)
	    inc hl
	    ld b,(hl)
	    inc hl
	    push hl
	     or a
	     ld a,' '
	     jr z,_
	     ld a,'*'
_
	     call PutCharXY
	     ACALL(PutStringFormat)
	    pop hl
	   pop de
	   xor a
	   ld c,a
	   cpir
	  pop de
	 pop bc
	 djnz draw_menu_loop
	pop af
	ld c,a
	jr z,draw_rom_list
	sub e
	ld (menuNextItem),a
	ld a,c
	scf
	sbc a,d
	ld (menuPrevItem),a
	; Special handling for controls menu
	ld a,(current_menu)
	cp ControlsMenuIndex
	ret nz
	ld a,c
	cp 5
	jr z,_
	xor 4^9
	cp 4
	jr z,++_
	cp 9
	ret nz
	ld (menuNextItem),a
	ret
_
	ld a,16
_
	ld (menuPrevItem),a
	ret

draw_rom_list:
	; Draw the ROM list
	dec c
	ld a,(romListFrameCount)
	ld b,a
	jr z,_
	push bc
	 ld hl,romListItem
	 ld (current_item_ptr),hl
	 APTR(LoadRomHelpText)
	 ld a,MAGENTA
	 ld bc,1<<8|205
	 ACALL(PutStringColorXY)
	pop bc
	ld a,c
_
	ld (menuPrevItem),a

	ld a,c
	inc a
	cp b
	jr nz,_
	xor a
_
	inc a
	ld (menuNextItem),a
	
	ld a,(romListFrameStart)
	ld e,a
	add a,c
	dec a
	ld (romListItem+1),a
	ld d,3
	mlt de
	ld iy,romListStart
	add iy,de
	ld hl,1<<8|25
	ld (cursorRowCol),hl
	djnz draw_rom_list_loop
	APTR(LoadRomNoRomsText)
	ld a,GRAY
	AJUMP(PutStringColor)
	
draw_rom_list_loop:
	dec c
	push bc
	 ld a,OLIVE
	 jr nz,_
	 ld a,WHITE
_
	 call GetRomDescription
	 ACALL(PutNStringColor)
	 ACALL(PutNewLine)
	pop bc
	lea iy,iy+3
	djnz draw_rom_list_loop
	ret
	
draw_mini_screen:
	ld hl,mini_frame_backup
	ld de,menu_frame_buffer + (320*55 - 8)
	ld a,144
draw_mini_screen_row_loop:
	ld bc,160
	ex de,hl
	add hl,bc
	ex de,hl
	ldir
	dec a
	jr nz,draw_mini_screen_row_loop
	ret
	
	; Returns key code in A, or 0 if ON is pressed.
	; Intercepts configured buttons for brightness changes.
	; Translates configured A and B/Menu buttons to Enter and Clear.
	; Also returns Z flag set if ON is pressed.
WaitForKey:
_
	call poll_single_key
	jr nz,-_
wait_for_key_loop:
	call poll_single_key
	jr z,wait_for_key_loop
	; Disallow translation of del/enter/clear or arrow keys
	cp 9+1
	jr c,_
	cp 15
	jr z,_
	cp 56
	jr z,_
	ld hl,BrightnessUpKey
	cp (hl)
	jr z,wait_for_key_increase_brightness
	inc hl ;BrightnessDownKey
	cp (hl)
	jr z,wait_for_key_decrease_brightness
	ld hl,AKey
	cp (hl)
	jr z,wait_for_key_force_enter
	inc hl ;BKey
	cp (hl)
	jr z,wait_for_key_force_clear
	inc hl ;SelectKey
	inc hl ;StartKey
	inc hl ;MenuKey
	cp (hl)
	jr z,wait_for_key_force_clear
_
	or a
	ret

wait_for_key_increase_brightness:
	scf
wait_for_key_decrease_brightness:
	sbc a,a
	add a,a
	inc a
	ld hl,mpBlLevel
	add a,(hl)
	jr z,wait_for_key_loop
	ld (hl),a
	jr wait_for_key_loop

wait_for_key_force_enter:
	ld a,9
	or a
	ret

wait_for_key_force_clear:
	ld a,15
	or a
	ret

GetKeyCode:
	ld c,0
	ld b,56
	ld hl,mpKeypadGrp0
GetKeyCodeLoop:
	ld a,b
	and 7
	jr nz,_
	inc hl
	inc hl
	ld e,(hl)
_
	sla e
	jr nc,_
	xor a
	cp c
	ret nz
	ld c,b
_
	djnz GetKeyCodeLoop
	ld a,c
	ret

; @param[in] a option index
; @param[out] a option value
; @param[out] ubc pointer to option value in current config
; @param[out] uhl pointer to length-prefixed sequence of display strings for
;                 option values
; @param[out] zf option value not game-specific
GetOption:
	ld hl,current_config
	ld bc,0
	inc a
	ld c,a
	ld a,(hl)
	jr z,++_
	or a
	ld hl,(global_config_start)
	add hl,bc
	ld a,(hl)
	jr z,++_
	ld hl,(game_config_start)
	add hl,bc
	ld b,a
	ld a,(hl)
	cp -1
	jr nz,_
	ld a,b
_
	ld b,0
_
	push hl
	 ld hl,OptionList-2
	 add hl,bc
	 add hl,bc
	 ld bc,(ArcBase)
	 add hl,bc
	 ld hl,(hl)
	 dec.s hl
	 add hl,bc
	pop bc
	ret

ItemDisplayDigit:
	cp 2
	ld a,(current_state)
	jr nz,_
	xor a
	ACALL(GetOption)
	ld c,0
	jr z,_
	inc c
_
	or a
	sbc hl,hl
	ld l,a
ItemDisplayLink:
ItemDisplayCmd:
ItemChangeLink:
ItemChangeCmd:
ItemDeleteLink:
ItemDeleteCmd:
	ret
	
ItemDisplayKey:
	ACALL(GetOption)
	ld c,0
	jr z,_
	inc c
_
	ld hl,(calcType)
	dec l
	jr z,_
	add a,57
_
	push de
	 APTR(KeyNames)
	pop de
	jr ItemDisplayKeyEntry

; @param[in] a option index
; @param[out] a 0
; @param[out] b 0
; @param[out] c nonzero iff option value is game-specific
; @param[out] uhl pointer to display string for option value
ItemDisplayOption:
	ACALL(GetOption)
	ld c,0
	jr z,_
	inc c
_
	inc hl

; @param[in] a option value
; @param[in] uhl pointer to sequence of display strings for option values
; @param[out] a 0
; @param[out] b 0
; @param[out] uhl pointer to display string for option value
ItemDisplayKeyEntry:
	ld b,a
	or a
	ret z
	xor a
_
	cp (hl)
	inc hl
	jr nz,-_
	djnz -_
	ret
	
ConfirmText:
	.db "No\n",0
	.db "Yes",0
	
ConfirmDeleteROM:
	.db "Delete ROM files for %s?",0
	
ConfirmDeleteState:
	.db "Delete state slot %c?",0
	
ConfirmLoadState:
	.db "Load state slot %c?",0
	
ConfirmSaveState:
	.db "Overwrite state slot %c?",0
	
ConfirmRestartGame:
	.db "Restart the Game Boy system?",0
	
TitleChecksumFormat:
	.db "%.16s  %04X",0
	
MenuList:
MainMenuIndex = ($-MenuList)/2
	.dw MainMenu+1
LoadGameMenuIndex = ($-MenuList)/2
	.dw LoadGameMenu+1
GraphicsMenuIndex = ($-MenuList)/2
	.dw GraphicsMenu+1
ControlsMenuIndex = ($-MenuList)/2
	.dw ControlsMenu+1
EmulationMenuIndex = ($-MenuList)/2
	.dw EmulationMenu+1
	
	.dw OptionConfigSelect+1
OptionList:
	.dw 1 ;OptionFrameskipValue+1
	.dw OptionFrameskipType+1
	.dw OptionSpeedDisplay+1
	.dw OptionAutoSaveState+1
	.dw OptionPaletteSelection+1
	.dw OptionTimeZone+1
	.dw OptionDST+1
	.dw OptionScalingMode+1
	.dw OptionSkinDisplay+1
	.dw OptionTurboMode+1
	.dw OptionScaleTracking+1
	.dw OptionMessageDisplay+1
	.dw OptionAdjustColors+1
	.dw OptionConfirmState+1
	.dw OptionPreferredModel+1
	.dw OptionScalingMethod+1
	
CmdList:
ReturnExitReason = ($-CmdList)/2
	.dw CmdExit+1
; SelectRomExitReason = ($-CmdList)/2
	.dw CmdExit+1
ExitExitReason = ($-CmdList)/2
	.dw CmdExit+1
RestartExitReason = ($-CmdList)/2
	.dw CmdRestart+1
	
ItemDisplayCallbacks:
	.dw ItemDisplayLink+1
	.dw ItemDisplayCmd+1
	.dw ItemDisplayDigit+1
	.dw ItemDisplayOption+1
	.dw ItemDisplayKey+1
	; No ROM item display callback
	
ItemChangeCallbacks:
	.dw ItemChangeLink+1
	.dw ItemChangeCmd+1
	.dw ItemChangeDigit+1
	.dw ItemChangeOption+1
	.dw ItemChangeKey+1
	.dw ItemChangeRom+1
	
ItemSelectCallbacks:
	.dw ItemSelectLink+1
	.dw ItemSelectCmd+1
	.dw ItemSelectDigit+1
	.dw ItemSelectOption+1
	.dw ItemSelectKey+1
	.dw ItemSelectRom+1
	
ItemDeleteCallbacks:
	.dw ItemDeleteLink+1
	.dw ItemDeleteCmd+1
	.dw ItemDeleteDigit+1
	.dw ItemDeleteOption+1
	.dw ItemDeleteKey+1
	.dw ItemDeleteRom+1


#if 0
MenuTemplate:
	.db item_count

	.db title_x,title_y
	.db title_text,0

	.db item_0_help_text
	.db item_0_type
	.db item_0_type_data
	.db item_0_x,item_0_y
	.db item_0_text,0

	.db item_1_help_text
; ...
#endif

MainMenu:
	.db 10

VersionLengthBegin:
	.db VERSION
VersionLength = $ - VersionLengthBegin
	.seek VersionLengthBegin
	.org VersionLengthBegin
	.db 5, 11 - (VersionLength / 2)
	.db "TI-Boy CE Alpha ",VERSION,"\nhttps://calc84maniac.github.io/tiboyce",0

	.db "Select to load the game state from the\ncurrent slot for this game.\nPress left/right to change the slot.",0
	.db ITEM_DIGIT | ITEM_ROMONLY
	.db 0
	.db 55,0
	.db "Load State Slot %c",0

	.db "Select to save the game state to the\ncurrent slot for this game.\nPress left/right to change the slot.",0
	.db ITEM_DIGIT | ITEM_ROMONLY
	.db 1
	.db 65,0
	.db "Save State Slot %c",0

	.db "Choose configuration options to edit.\nGame-specific options show a '*'.\nPress DEL to revert an option.",0
	.db ITEM_OPTION | ITEM_ROMONLY
	.db -1
	.db 85,0
	.db "Config: %-8s",0

	.db "Select to set appearance and\nframeskip behavior.",0
	.db ITEM_LINK
	.db GraphicsMenuIndex
	.db 105,0
	.db "Graphics Options",0

	.db "Select to change the in-game behavior\nof buttons and arrow keys.",0
	.db ITEM_LINK
	.db ControlsMenuIndex
	.db 115,0
	.db "Control Options",0

	.db "Select to manage miscellaneous options.",0
	.db ITEM_LINK
	.db EmulationMenuIndex
	.db 125,0
	.db "Emulation Options",0

	.db "Select to load a new game\n(will exit a currently playing game).",0
	.db ITEM_LINK
	.db LoadGameMenuIndex
	.db 145,0
	.db "Load new game",0

	.db "Select to reset the Game Boy\nwith the current game loaded.",0
	.db ITEM_CMD | ITEM_ROMONLY
	.db RestartExitReason
	.db 155,0
	.db "Restart game",0

	.db "Select to exit this menu and\nresume gameplay.",0
	.db ITEM_CMD | ITEM_ROMONLY
	.db ReturnExitReason
	.db 165,0
	.db "Return to game",0

	.db "Select to exit the emulator and\nreturn to TI-OS.",0
	.db ITEM_CMD
	.db ExitExitReason
	.db 185,0
	.db "Exit TI-Boy CE",0
	
NoRomLoadedDescription:
	.db _-$-1,"No ROM currently loaded"
_
	
LoadGameMenu:
	.db 1

	.db 10,14
	.db "Load New ROM",0

	.db "Return to the main menu.",0
	.db ITEM_LINK
	.db MainMenuIndex
	.db 185,0
	.db "Back",0
	
LoadRomHelpText:
	.db "Press 2nd/Enter to start the game.\nPress left/right to scroll pages.\nPress DEL to delete ROM files.",0
	
LoadRomNoRomsText:
	.db "No ROMs found!",0
	
GraphicsMenu:
	.db 9

	.db 10,12
	.db "Graphics Options",0

	.db "",0
	.db ITEM_OPTION
	.db ScalingModeOffset
	.db 55,0
	.db "Scaling mode: %-10s",0

	.db "Nearest: No blending, sharp pixels.\nLinear: Flicker-based blending, slower\nbut with more consistent pixel size.",0
	.db ITEM_OPTION
	.db ScalingMethodOffset
	.db 65,0
	.db "Scaling method: %-7s",0

	.db "Static: Scale relative to the screen.\nScrolling: Scale relative to tilemaps,\nwhich may reduce shimmering on scroll.",0
	.db ITEM_OPTION
	.db ScaleTrackingOffset
	.db 75,0
	.db "Scale tracking: %-9s",0

	.db "Display a skin in \"no scaling\" mode.\nRequires the TIBoySkn.8xv AppVar.",0
	.db ITEM_OPTION
	.db SkinDisplayOffset
	.db 85,0
	.db "Skin display: %-3s",0

	.db "Off: Do not skip any frames.\nAuto: Skip up to N frames as needed.\nManual: Render 1 of each N+1 frames.",0
	.db ITEM_OPTION
	.db FrameskipTypeOffset
	.db 105,0
	.db "Frameskip type: %-6s",0

	.db "",0
	.db ITEM_DIGIT
	.db 2
	.db 115,0
	.db "Frameskip value: %u",0

	.db "Default: Use GBC game-specific palette.\nOthers: Use GBC manual palette.",0
	.db ITEM_OPTION
	.db PaletteSelectionOffset
	.db 135,0
	.db "GB palette selection: %-10s",0

	.db "Off: Use specified colors directly.\nGBC: Adjust to emulate a GBC display.\nGBA: Adjust to emulate a GBA display.",0
	.db ITEM_OPTION
	.db AdjustColorsOffset
	.db 145,0
	.db "Adjust colors: %-3s",0

	.db "Return to the main menu.",0
	.db ITEM_LINK
	.db MainMenuIndex
	.db 185,0
	.db "Back",0
	
ControlsMenu:
	.db 16

	.db 10,12
	.db "Control Options",0

	.db "",0
	.db ITEM_KEY
	.db RightKeyOffset
	.db  55,0
	.db "Right: %-9s",0

	.db "",0
	.db ITEM_KEY
	.db LeftKeyOffset
	.db  65,0
	.db "Left:  %-9s",0

	.db "",0
	.db ITEM_KEY
	.db UpKeyOffset
	.db  75,0
	.db "Up:    %-9s",0

	.db "",0
	.db ITEM_KEY
	.db DownKeyOffset
	.db  85,0
	.db "Down:  %-9s",0

	.db "",0
	.db ITEM_KEY
	.db AKeyOffset
	.db 55,20
	.db "A:      %-9s",0

	.db "",0
	.db ITEM_KEY
	.db BKeyOffset
	.db 65,20
	.db "B:      %-9s",0

	.db "",0
	.db ITEM_KEY
	.db SelectKeyOffset
	.db 75,20
	.db "Select: %-9s",0

	.db "",0
	.db ITEM_KEY
	.db StartKeyOffset
	.db 85,20
	.db "Start:  %-9s",0

	.db "Open the emulator menu.",0
	.db ITEM_KEY
	.db MenuKeyOffset
	.db 105,0
	.db "Open menu:       %-9s",0

	.db "Enable or toggle turbo mode.\nPress DEL to unmap this key.",0
	.db ITEM_KEY
	.db TurboKeyOffset
	.db 115,0
	.db "Turbo mode:      %-9s",0

	.db "Save state to the current slot.\nPress DEL to unmap this key.",0
	.db ITEM_KEY
	.db SaveStateKeyOffset
	.db 125,0
	.db "Save state:      %-9s",0

	.db "Load state from the current slot.\nPress DEL to unmap this key.",0
	.db ITEM_KEY
	.db LoadStateKeyOffset
	.db 135,0
	.db "Load state:      %-9s",0

	.db "Show or select the current state slot.\nPress a number while holding to select.\nPress DEL to unmap this key.",0
	.db ITEM_KEY
	.db StateKeyOffset
	.db 145,0
	.db "State slot:      %-9s",0

	.db "Turn screen brightness up.\nPress DEL to unmap this key.",0
	.db ITEM_KEY
	.db BrightnessUpKeyOffset
	.db 155,0
	.db "Brightness up:   %-9s",0

	.db "Turn screen brightness down.\nPress DEL to unmap this key.",0
	.db ITEM_KEY
	.db BrightnessDownKeyOffset
	.db 165,0
	.db "Brightness down: %-9s",0

	.db "Return to the main menu.",0
	.db ITEM_LINK
	.db MainMenuIndex
	.db 185,0
	.db "Back",0
	
EmulationMenu:
	.db 9

	.db 10,11
	.db "Emulation Options",0

	.db "Preferred Game Boy model to emulate.\nGBC will only be used if compatible.\nRequires game restart to take effect.",0
	.db ITEM_OPTION
	.db PreferredModelOffset
	.db 55,0
	.db "Preferred model: %-15s",0

	.db "Automatically save state on ROM exit.\nState will be resumed upon next load.",0
	.db ITEM_OPTION
	.db AutoSaveStateOffset
	.db 75,0
	.db "Auto save state: %-3s",0

	.db "",0
	.db ITEM_OPTION
	.db ConfirmStateOperationOffset
	.db 85,0
	.db "Confirm state save/load: %-9s",0

	.db "Display emulator message overlays.",0
	.db ITEM_OPTION
	.db MessageDisplayOffset
	.db 95,0
	.db "Message display: %-3s",0

	.db "",0
	.db ITEM_OPTION
	.db TurboModeOffset
	.db 115,0
	.db "Turbo mode: %-6s",0

	.db "Show percentage of real GB performance.\nTurbo: Display when turbo is activated.\nSlowdown: Display when below fullspeed.",0
	.db ITEM_OPTION
	.db SpeedDisplayOffset
	.db 125,0
	.db "Speed display: %-8s",0

	.db "The time offset for games with clocks.\nShould match the time set in the OS.\nRelevant when sharing save files.",0
	.db ITEM_OPTION
	.db TimeZoneOffset
	.db 145,0
	.db "Time zone: UTC%-6s",0

	.db "Set to on if DST is currently active.",0
	.db ITEM_OPTION
	.db DaylightSavingTimeOffset
	.db 155,0
	.db "Daylight Saving Time: %-3s",0

	.db "Return to the main menu.",0
	.db ITEM_LINK
	.db MainMenuIndex
	.db 185,0
	.db "Back",0
	
OptionPreferredModel:
	.db 3
	.db "Game Boy",0
	.db "Game Boy Color",0
	.db "GBA back-compat",0
	
OptionConfigSelect:
	.db 2
	.db "global",0
	.db "per-game",0
	
OptionFrameskipType:
	.db 3
	.db "manual",0
	.db "auto",0
	.db "off",0
	
OptionScalingMode:
	.db 2
	.db "no scaling",0
	.db "fullscreen",0
	
OptionAutoSaveState:
OptionDST:
OptionSkinDisplay:
OptionMessageDisplay:
	.db 2
	.db "off",0
	.db "on",0
	
OptionAdjustColors:
	.db 3
	.db "off",0
	.db "GBC",0
	.db "GBA",0
	
OptionConfirmState:
	.db 4
	.db "never",0
	.db "load only",0
	.db "overwrite",0
	.db "both",0
	
OptionTurboMode:
	.db 2
	.db "toggle",0
	.db "hold",0
	
OptionSpeedDisplay:
	.db 4
	.db "never",0
	.db "turbo",0
	.db "slowdown",0
	.db "always",0
	
OptionScalingMethod:
	.db 2
	.db "nearest",0
	.db "linear",0
	
OptionScaleTracking:
	.db 2
	.db "static",0
	.db "scrolling",0
	
OptionPaletteSelection:
	.db 14
	.db "default",0
	.db "grayscale",0
	.db "brown",0
	.db "pastel mix",0
	.db "blue",0
	.db "green",0
	.db "red",0
	.db "orange",0
	.db "dark blue",0
	.db "dark green",0
	.db "dark brown",0
	.db "yellow",0
	.db "inverted",0
	.db "classic",0
	
OptionTimeZone:
	.db 32
	.db "",0
	.db "+1:00",0
	.db "+2:00",0
	.db "+3:00",0
	.db "+3:30",0
	.db "+4:00",0
	.db "+4:30",0
	.db "+5:00",0
	.db "+5:30",0
	.db "+6:00",0
	.db "+6:30",0
	.db "+7:00",0
	.db "+8:00",0
	.db "+9:00",0
	.db "+9:30",0
	.db "+10:00",0
	.db "+11:00",0
	.db "+12:00",0
	.db "+13:00",0
	.db "-12:00",0
	.db "-11:00",0
	.db "-10:00",0
	.db "-9:00",0
	.db "-8:00",0
	.db "-7:00",0
	.db "-6:00",0
	.db "-5:00",0
	.db "-4:00",0
	.db "-3:30",0
	.db "-3:00",0
	.db "-2:00",0
	.db "-1:00",0
	
KeyNames:
	; 83 Premium CE keys
	.db "(appuyer)",0
	.db "bas",0
	.db "gauche",0
	.db "droite",0
	.db "haut",0
	.db 0,0,0,0
	.db "entrer",0
	.db "+",0
	.db "-",0
	.db "x",0
	.db "div",0
	.db "^",0
	.db "annul",0
	.db 0
	.db "(-)",0
	.db "3",0
	.db "6",0
	.db "9",0
	.db ")",0
	.db "frac",0
	.db "var",0
	.db 0
	.db ".",0
	.db "2",0
	.db "5",0
	.db "8",0
	.db "(",0
	.db "resol",0
	.db "prgm",0
	.db "stats",0
	.db "0",0
	.db "1",0
	.db "4",0
	.db "7",0
	.db ",",0
	.db "trig",0
	.db "matrice",0
	.db "XT0n",0
	.db "(rien)",0
	.db "sto>",0
	.db "ln",0
	.db "log",0
	.db "x^2",0
	.db "<>",0
	.db "math",0
	.db "alpha",0
	.db "graphe",0
	.db "trace",0
	.db "zoom",0
	.db "fenetre",0
	.db "f(x)",0
	.db "2nde",0
	.db "mode",0
	.db "suppr",0
	; 84 Plus CE keys
	.db "(press)",0
	.db "down",0
	.db "left",0
	.db "right",0
	.db "up",0
	.db 0,0,0,0
	.db "enter",0
	.db "+",0
	.db "-",0
	.db "x",0
	.db "div",0
	.db "^",0
	.db "clear",0
	.db 0
	.db "(-)",0
	.db "3",0
	.db "6",0
	.db "9",0
	.db ")",0
	.db "tan",0
	.db "vars",0
	.db 0
	.db ".",0
	.db "2",0
	.db "5",0
	.db "8",0
	.db "(",0
	.db "cos",0
	.db "prgm",0
	.db "stat",0
	.db "0",0
	.db "1",0
	.db "4",0
	.db "7",0
	.db ",",0
	.db "sin",0
	.db "apps",0
	.db "XT0n",0
	.db "(none)",0
	.db "sto>",0
	.db "ln",0
	.db "log",0
	.db "x^2",0
	.db "x^-1",0
	.db "math",0
	.db "alpha",0
	.db "graph",0
	.db "trace",0
	.db "zoom",0
	.db "window",0
	.db "y=",0
	.db "2nd",0
	.db "mode",0
	.db "del",0
	
KeySMCList:
	.db key_smc_right - key_smc_turbo - 1
	.db key_smc_left - key_smc_right - 1
	.db key_smc_up - key_smc_left - 1
	.db key_smc_down - key_smc_up - 1
	.db key_smc_a - key_smc_down - 1
	.db key_smc_b - key_smc_a - 1
	.db key_smc_select - key_smc_b - 1
	.db key_smc_start - key_smc_select - 1
	.db key_smc_menu - key_smc_start - 1
	.db key_smc_save_state - key_smc_menu - 1
	.db key_smc_load_state - key_smc_save_state - 1
	.db key_smc_state_slot - key_smc_load_state - 1
	.db key_smc_brightness_up - key_smc_state_slot - 1
	.db key_smc_brightness_down - key_smc_brightness_up - 1
	
TimeZoneOffsetTable:
	.db $00,$10,$20,$30,$38,$40,$48,$50,$58,$60,$68,$70,$80,$90,$98,$A0,$B0,$C0,$D0
	.db $C0,$B0,$A0,$90,$80,$70,$60,$50,$40,$38,$30,$20,$10
	
ManualPaletteIndexTable:
	.db $16,$12,$17,$B8,$05,$B0,$07,$AD,$7C,$79,$BA,$13
	.db $1D ;Classic
	
PaletteIndex:
	.db $80,$B0,$40, $88,$20,$68, $DE,$00,$70, $DE,$20,$78
	.db $20,$20,$38, $20,$B0,$90, $20,$B0,$A0, $E0,$B0,$C0
	.db $98,$B6,$48, $80,$E0,$50, $1E,$1E,$58, $20,$B8,$E0
	.db $88,$B0,$10, $20,$00,$10, $20,$E0,$18, $E0,$18,$00
	.db $18,$E0,$20, $A8,$E0,$20, $18,$E0,$00, $20,$18,$D8
	.db $C8,$18,$E0, $00,$E0,$40, $28,$28,$28, $18,$E0,$60
	.db $20,$18,$E0, $00,$00,$08, $E0,$18,$30, $D0,$D0,$D0
	.db $20,$E0,$E8, $F0,$F0,$F0 ; Classic
	
PaletteDictionary:
	.dw $7FFF,$32BF,$00D0,$0000
	.dw $639F,$4279,$15B0,$04CB
	.dw $7FFF,$6E31,$454A,$0000
	.dw $7FFF,$1BEF,$0200,$0000
	.dw $7FFF,$421F,$1CF2,$0000
	.dw $7FFF,$5294,$294A,$0000
	.dw $7FFF,$03FF,$012F,$0000
	.dw $7FFF,$03EF,$01D6,$0000
	.dw $7FFF,$42B5,$3DC8,$0000
	.dw $7E74,$03FF,$0180,$0000
	.dw $67FF,$77AC,$1A13,$2D6B
	.dw $7ED6,$4BFF,$2175,$0000
	.dw $53FF,$4A5F,$7E52,$0000
	.dw $4FFF,$7ED2,$3A4C,$1CE0
	.dw $03ED,$7FFF,$255F,$0000
	.dw $036A,$021F,$03FF,$7FFF
	.dw $7FFF,$01DF,$0112,$0000
	.dw $231F,$035F,$00F2,$0009
	.dw $7FFF,$03EA,$011F,$0000
	.dw $299F,$001A,$000C,$0000
	.dw $7FFF,$027F,$001F,$0000
	.dw $7FFF,$03E0,$0206,$0120
	.dw $7FFF,$7EEB,$001F,$7C00
	.dw $7FFF,$3FFF,$7E00,$001F
	.dw $7FFF,$03FF,$001F,$0000
	.dw $03FF,$001F,$000C,$0000
	.dw $7FFF,$033F,$0193,$0000
	.dw $0000,$4200,$037F,$7FFF
	.dw $7FFF,$7E8C,$7C00,$0000
	.dw $7FFF,$1BEF,$6180,$0000
	.dw $4778,$3290,$1D87,$0861 ; Classic
	
	; Green adjustment offset generation data
	; Each row corresponds to the three upper bits of the blue component,
	; and contains a starting green offset followed by a bitmap, where
	; each 1 in the bitmap means subtracting one from the offset.
GreenAdjustOffsetsGBC:
#ifdef BALANCED_COLOR_ADJUST
	; Gamma 1.6
	.db  0 << 4, %00001000, %00000100, %00100100, %10101111
	.db  3 << 4, %01101000, %10000010, %00010010, %10101101
	.db  5 << 4, %01101100, %10001000, %01001001, %00110111
	.db  7 << 4, %01110101, %01001000, %10001001, %00101111
	.db  9 << 4, %01111010, %10101000, %10001001, %00101101
	.db 10 << 4, %01110110, %10101000, %10001000, %10010111
	.db 11 << 4, %01110111, %01010001, %00010000, %10001010
	.db 12 << 4, %01111110, %10101000, %10000100, %00010000
#else
	; Gamma 2.2
	.db  0 << 4, %00000010, %00000001, %00001000, %10101101
	.db  3 << 4, %01101000, %01000000, %10000100, %10101011
	.db  6 << 4, %01111010, %10001000, %00100010, %01010111
	.db  8 << 4, %01101110, %11001000, %10001000, %10101011
	.db 11 << 4, %01111111, %01100100, %10001000, %10011011
	.db 12 << 4, %01111101, %11010100, %10001000, %10010110
	.db 13 << 4, %01111101, %11010100, %10010000, %10001010
	.db 14 << 4, %01111111, %01101010, %01000010, %00010000
#endif
	
GreenAdjustOffsetsGBA:
#ifdef BALANCED_COLOR_ADJUST
	; Gamma 1.6
	.db  0 << 4, %00000001, %00000000, %00100000, %00000100
	.db  2 << 4, %01001000, %00100000, %00010000, %00000010
	.db  4 << 4, %01101001, %00000100, %00000010, %00000000
	.db  5 << 4, %00110101, %00010000, %01000000, %01000000
	.db  7 << 4, %01101100, %10010001, %00000100, %00000100
	.db  9 << 4, %01110110, %10010010, %00010000, %01000000
	.db 10 << 4, %01011101, %01010010, %00100001, %00000100
	.db 12 << 4, %01110110, %10101001, %00100010, %00010000
#else
	; Gamma 2.2
	.db  0 << 4, %00000000, %10000000, %00000000, %10000000
	.db  3 << 4, %01101000, %00100000, %00000000, %10000000
	.db  5 << 4, %01110100, %10000010, %00000000, %00100000
	.db  7 << 4, %01111010, %10010000, %01000000, %00001000
	.db  9 << 4, %01111011, %01001001, %00000010, %00000001
	.db 11 << 4, %01111011, %10101001, %00010000, %01000000
	.db 13 << 4, %01111011, %10110100, %10010001, %00000100
	.db 15 << 4, %01110111, %11010110, %01010001, %00010000
#endif
	
spiGammaCommandDescriptors:
	.db 14,$E0 ; Positive gamma
	.db 14,$E1 ; Negative gamma
	.db -1
	
spiGammaUniform:
	; Positive gamma
	#define J0 1
	#define J1 3
	SPI_GAMMA(129, 125, 121, 83, 65, 45, 41, 10,  49, 38, 13,  16, 6,  43, 20, 11)
	#undef J1
	#undef J0
	; Negative gamma
	#define J0 0
	#define J1 3
	SPI_GAMMA(129, 125, 121, 85, 64, 45, 41, 10,  49, 38, 14,  17, 7,  43, 20, 11)
	#undef J1
	#undef J0
spiGammaCmdSize = $ - spiGammaUniform
	; Menu colors
	.dw $1882 ;BLUE
	.dw $EA56 ;MAGENTA
	.dw $CA8B ;OLIVE
	.dw $4210 ;GRAY
spiGammaConfigSize = $ - spiGammaUniform
menuPaletteSize = spiGammaConfigSize - spiGammaCmdSize
	
spiGammaGBC:
	; Positive gamma
	#define J0 0
	#define J1 1
	SPI_GAMMA(129, 124, 123, 83, 57, 24, 19, 10,  55, 42, 16,  16, 6,  50, 37, 25)
	#undef J1
	#undef J0
	; Negative gamma
	#define J0 2
	#define J1 1
	SPI_GAMMA(129, 124, 123, 83, 56, 23, 18, 10,  55, 42, 16,  16, 7,  51, 38, 27)
	#undef J1
	#undef J0
	; Menu colors
	.dw $1CA3 ;BLUE
	.dw $55F1 ;MAGENTA
	.dw $BE0A ;OLIVE
	.dw $35AD ;GRAY
	
spiGammaGBA:
	; Positive gamma
	#define J0 0
	#define J1 1
	SPI_GAMMA(129, 127, 126, 90, 68, 46, 44, 10,  56, 44, 16,  16, 7,  43, 20, 10)
	#undef J1
	#undef J0
	; Negative gamma
	#define J0 2
	#define J1 1
	SPI_GAMMA(129, 126, 126, 89, 67, 46, 44, 10,  56, 45, 17,  17, 7,  44, 20, 11)
	#undef J1
	#undef J0
	; Menu colors
	.dw $24C4 ;BLUE
	.dw $EE98 ;MAGENTA
	.dw $56CE ;OLIVE
	.dw $4A52 ;GRAY
	
spiGammaOriginal:
	; Positive gamma
	#define J0 1
	#define J1 3
	SPI_GAMMA(129, 128, 128, 83, 65, 45, 41, 10,  41, 32, 11,  16, 6,  43, 20, 11)
	#undef J1
	#undef J0
	; Negative gamma
	#define J0 0
	#define J1 3
	SPI_GAMMA(129, 128, 128, 85, 64, 45, 41, 10,  41, 32, 12,  17, 7,  43, 20, 11)
	#undef J1
	#undef J0
	; Menu colors
	.dw $1882 ;BLUE
	.dw $EA56 ;MAGENTA
	.dw $CA8B ;OLIVE
	.dw $4210 ;GRAY
	