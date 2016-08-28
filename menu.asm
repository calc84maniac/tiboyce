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
	 
	 ld hl,MainMenu
	 ld (current_menu),hl
	 ld a,1
	 ld (current_menu_selection),a
	 
	 call draw_current_menu
	
	 ld hl,(mpLcdBase)
	 push hl
	  ld hl,(current_buffer)
	  ld (mpLcdBase),hl
	  
menu_loop:
	  call WaitForKey
	  ld hl,(current_menu)
	  cp 1
	  jr z,menu_down
	  cp 4
	  jr z,menu_up
	  cp 15
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
	ld a,(current_menu_selection)
	dec a
	jr nz,_
	ld a,(hl)
_
	ld (current_menu_selection),a
	call draw_current_menu
	jr menu_loop
	
menu_down:
	ld a,(current_menu_selection)
	cp (hl)
	jr nz,_
	xor a
_
	inc a
	ld (current_menu_selection),a
	call draw_current_menu
	jr menu_loop
	
	
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
	
current_menu = $+1
	ld hl,0
current_menu_selection = $+1
	ld c,0
	; HL = menu structure, C = highlighted item index
draw_menu:
	ld b,(hl)
	inc hl
	inc b
	push bc
	 jr draw_menu_title
draw_menu_loop:
	dec c
	push bc
	 jr z,_
	 xor a
	 cpir
	 ld c,OLIVE
	 jr ++_
_
	 ld a,205
	 ld (cursorRow),a
	 ld a,1
	 ld (cursorCol),a
	 ld a,MAGENTA
	 call PutStringColor
draw_menu_title:
	 ld c,WHITE
_
	 ld a,(hl)
	 inc hl
	 ld (cursorRow),a
	 ld a,(hl)
	 inc hl
	 ld (cursorCol),a
	 ld a,c
	 call PutStringColor
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
	
MainMenu:
	.db 9
	.db 5,10,"TI-Boy CE Alpha 0.01",0
	.db "Select to set appearance and\n frameskip behavior.",0
	.db 50,1,"Graphics Options",0
	.db "Select to load the game state from the\n current slot for this game.\n Press left/right to change the slot.",0
	.db 70,1,"Load State: Slot ",0
	.db "Select to save the game state to the\n current slot for this game.\n Press left/right to change the slot.",0
	.db 80,1,"Save State: Slot ",0
	.db "Select to change the in-game behavior\n of buttons and arrow keys.",0
	.db 100,1,"Control Options",0
	.db "Select to manage miscellaneous options.",0
	.db 120,1,"Emulation Options",0
	.db "Select to load a new game\n (will exit a currently playing game).",0
	.db 140,1,"Load new game",0
	.db "Select to reset the Game Boy\n with the current game loaded.",0
	.db 150,1,"Restart game",0
	.db "Select to exit this menu and\n resume gameplay.",0
	.db 160,1,"Return to game",0
	.db "Select to exit the emulator and\n return to TI-OS.",0
	.db 180,1,"Exit TI-Boy CE",0