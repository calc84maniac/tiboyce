port_read = heapBot - 6
port_write = heapBot - 3

port_unlock:
	call __frameset0
	di
	ld hl,$C978ED
	ld (port_read),hl
	inc h
	ld (port_write),hl
	APTR(port_unlockfinish)
	push hl
	pop iy
	ld sp,__indcall+7
	ld bc,$0022
	xor a
port_target = $+1
	jp 0

port_lock:
port_lock_value = $+1
	ld a,$7C
	ld bc,$0020
	call port_write
	ei
	ret
