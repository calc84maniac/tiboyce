; Copyright 2015-2022 Matt "MateoConLechuga" Waltz
; Copyright 2023 Brendan "calc84maniac" Fletcher
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice,
;    this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its contributors
;    may be used to endorse or promote products derived from this software
;    without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

	; Returns carry reset on success
port_setup:
	ld bc,$20000
	scf
	sbc hl,hl
	push hl
	pop de
port_setup_find_loop:
	inc hl
	ld a,$ED
	cpir
	jr nz,port_setup_finish
	ld a,(hl)
	dec hl
	cp $41
	jr z,port_setup_found_ed41
	cp $B4
	jr nz,port_setup_find_loop
	ld (port_new_target),hl
	ld de,port_new_unlock
	jr port_setup_find_loop
port_setup_found_ed41:
	ld (port_old_target),hl
	push hl
	pop ix
	bit 0,(ix+5)
	jr nz,port_setup_find_loop
	ld de,port_old_unlock
port_setup_finish:
	ld hl,(ArcBase)
	add hl,de
	ld (port_unlock_code),hl
	ret

port_old_unlock:
	call port_old_unlockhelper
port_unlockfinish:
	in0 a,($20)
	ld (port_lock_value),a
	ld a,port_read & $FF
	out0 ($20),a
	ld a,port_read >> 16
	out0 ($22),a
	ret

port_new_unlock:
	ld de,$D19881
	push de
	or a
	sbc hl,hl
	push hl
	ld de,$03D1
	push de
	push hl
	call port_new_unlockhelper
	ld hl,12
	add hl,sp
	ld sp,hl
	jr port_unlockfinish
