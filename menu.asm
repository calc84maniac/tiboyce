#define ITEM_LINK 0
#define ITEM_CMD 1
#define ITEM_STATE 2

emulator_menu:
	push bc
	 ld hl,(current_buffer)
	 push hl
	 pop de
	 inc de
	 ld bc,160*240-1
	 ld (hl),BLUE_BYTE
	 ldir
	
	 call draw_mini_screen
	 
	 ld a,20
	 ld (cursorRow),a
	 ld a,1
	 ld (cursorCol),a
current_description = $+1
	 ld hl,0
	 ld a,MAGENTA
	 call PutNStringColor
	 
	 ld a,30
	 ld (cursorRow),a
	 ld a,1
	 ld (cursorCol),a
	 ld ix,(rom_start)
	 ld bc,$0134
	 add ix,bc
	 ld b,(ix+$014E-$0134)
	 ld c,(ix+$014F-$0134)
	 push bc
	  push ix
	   ld hl,TitleChecksumFormat
	   push hl
	    call PutStringFormat
	   pop hl
	  pop hl
	 pop hl
	 
	 xor a
	 ld (current_menu),a
	 inc a
	 ld (current_menu_selection),a
	 
	 call draw_current_menu
	
	 ld hl,(mpLcdBase)
	 push hl
	  ld hl,(current_buffer)
	  ld (mpLcdBase),hl
	  
menu_loop:
	  call WaitForKey
	  call get_current_menu_selection
	  dec a
	  jr z,menu_down
	  cp 3
	  jr c,menu_left_right
	  jr z,menu_up
	  cp 15-1
	  jr nz,menu_loop
	  
menu_exit:
	  call GetKeyCode
	  or a
	  jr nz,menu_exit
	 pop hl
	 ld (mpLcdBase),hl
	pop bc
	ret
	
menu_up:
	ld a,c
	dec a
	jr nz,_
	ld a,(hl)
_
	ld (current_menu_selection),a
	call draw_current_menu
	jr menu_loop
	
menu_down:
	ld a,c
	cp (hl)
	jr nz,_
	xor a
_
	inc a
	ld (current_menu_selection),a
	call draw_current_menu
	jr menu_loop
	
menu_left_right:
	dec a
	add a,a
	dec a
current_item_ptr = $+1
	ld hl,0
	ld de,ItemChangeCallbacks
	call DoItemCallback
	jr menu_loop
	
get_current_menu_selection:
	ld hl,MenuList
current_menu = $+1
	ld c,0
	ld b,3
	mlt bc
	add hl,bc
	ld hl,(hl)
current_menu_selection = $+1
	ld c,0
	ret
	
draw_current_menu:
	ld hl,(current_buffer)
	ld de,160*205
	add hl,de
	push hl
	pop de
	inc de
	ld bc,160*30-1
	ld (hl),BLUE_BYTE
	ldir
	
	call get_current_menu_selection
	
	; HL = menu structure, C = highlighted item index
draw_menu:
	ld b,(hl)
	inc hl
	inc b
	push bc
	 ld a,WHITE
	 call SetStringColor
	 ex de,hl
	 jr draw_menu_title
draw_menu_loop:
	dec c
	push bc
	 jr z,_
	 xor a
	 cpir
	 ld a,OLIVE
	 jr ++_
_
	 ld a,205
	 ld (cursorRow),a
	 ld a,1
	 ld (cursorCol),a
	 ld a,MAGENTA
	 call PutStringColor
	 ld (current_item_ptr),hl
	 ld a,WHITE
_
	 call SetStringColor
	 
	 ld de,ItemDisplayCallbacks
	 call DoItemCallback
	 inc de
	 
draw_menu_title:
	 ld a,(de)
	 inc de
	 ld (cursorRow),a
	 ld a,(de)
	 inc de
	 ld (cursorCol),a
	 push hl
	  push de
	   call PutStringFormat
	  pop hl
	 pop de
	 xor a
	 ld c,a
	 cpir
	pop bc
	djnz draw_menu_loop
	ret
	
draw_mini_screen:
	ld hl,160*(120-72) + 80 - 4
	ld de,(current_buffer)
	add hl,de
	ld a,d
	xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	ld d,a
	ld bc,80*256 + 144
	ld a,2
draw_mini_screen_row_loop:
	push bc
	 ld c,a
draw_mini_screen_pixel_loop:
	 ld a,(de)
	 ld (hl),a
	 inc de
	 ld a,(de)
	 rld
	 inc de
	 inc hl
	 djnz draw_mini_screen_pixel_loop
	 ld a,c
	 ld c,80
	 add hl,bc
	 dec a
	 jr nz,_
	 ld a,3
	 jr ++_
_
	 ex de,hl
	 add hl,bc
	 add hl,bc
	 ex de,hl
_
	pop bc
	dec c
	jr nz,draw_mini_screen_row_loop
	ret
	
WaitForKey:
	call GetKeyCode
	or a
	jr nz,WaitForKey
_
	call GetKeyCode
	or a
	jr z,-_
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
	
DoItemCallback:
	ld c,(hl)
	inc hl
	ex de,hl
	ld b,3
	mlt bc
	add hl,bc
	ld b,a
	ld a,(de)
	ld hl,(hl)
	jp (hl)
	
ItemDisplayState:
	xor a
	sbc hl,hl
current_state = $+1
	ld l,0
ItemDisplayLink:
ItemDisplayCmd:
ItemChangeLink:
ItemChangeCmd:
	ret
	
ItemChangeState:
	ld hl,current_state
	ld a,(hl)
	add a,b
	cp 10
	jr c,_
	ld a,9
	jr nz,_
	xor a
_
	ld (hl),a
	jp draw_current_menu
	
TitleChecksumFormat:
	.db "%.16s  %04X",0
	
MenuList:
	.dl MainMenu
	
ItemDisplayCallbacks:
	.dl ItemDisplayLink
	.dl ItemDisplayCmd
	.dl ItemDisplayState
	
ItemChangeCallbacks:
	.dl ItemChangeLink
	.dl ItemChangeCmd
	.dl ItemChangeState
	
MainMenu:
	.db 9
	.db 5,10,"TI-Boy CE Alpha 0.01",0
	.db "Select to set appearance and\n frameskip behavior.",0
	.db ITEM_LINK,1, 50,1,"Graphics Options",0
	.db "Select to load the game state from the\n current slot for this game.\n Press left/right to change the slot.",0
	.db ITEM_STATE,0, 70,1,"Load State Slot %u",0
	.db "Select to save the game state to the\n current slot for this game.\n Press left/right to change the slot.",0
	.db ITEM_STATE,1, 80,1,"Save State Slot %u",0
	.db "Select to change the in-game behavior\n of buttons and arrow keys.",0
	.db ITEM_LINK,2, 100,1,"Control Options",0
	.db "Select to manage miscellaneous options.",0
	.db ITEM_LINK,3, 120,1,"Emulation Options",0
	.db "Select to load a new game\n (will exit a currently playing game).",0
	.db ITEM_CMD,0, 140,1,"Load new game",0
	.db "Select to reset the Game Boy\n with the current game loaded.",0
	.db ITEM_CMD,1, 150,1,"Restart game",0
	.db "Select to exit this menu and\n resume gameplay.",0
	.db ITEM_CMD,2, 160,1,"Return to game",0
	.db "Select to exit the emulator and\n return to TI-OS.",0
	.db ITEM_CMD,3, 180,1,"Exit TI-Boy CE",0