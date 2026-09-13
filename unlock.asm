	; Returns zero flag set on success
port_setup:
	ld hl,($000008+5)
	ld a,(hl)
	cp $CD
	ret nz
	inc hl
	ld de,(hl)
	ld hl,(_CheckIfEmulated+1)
	sbc hl,de
	ret nz

	ld hl,(_KeypadScanFull+1)
	ld bc,10
	add hl,bc
	ld (port_target),hl
	ld c,port_pattern_size
	call memcmp_inline
port_pattern:
	out (bc),a
	ld a,b
	cp $A0
	jr z,$+3
	rst 08h
port_pattern_size = $ - port_pattern

port_unlockfinish:
	ld sp,ix
	pop ix
	in0 a,($20)
	ld (port_lock_value),a
	ld a,port_read & $FF
	out0 ($20),a
	ld a,port_read >> 16
	out0 ($22),a
	ret
