	BITS 16
	jmp start
	version dw 0    ; 0x07C2
	jmp input       ; 0x07C4
	jmp delay       ; 0x07C6
	jmp tone        ; 0x07C8
	jmp mute        ; 0x07CB
	jmp clear       ; 0x07CE
	jmp sprite      ; 0x07D2
	jmp pixel       ; 0x07D5
	jmp set_palette ; 0x07D8
	jmp set_keymap  ; 0x07DB
start:
	mov ax, 09C0h ; Set up the stack
	mov ss, ax
	mov sp, 4096
	sub ax, 512 ; Set up the data segment
	mov ds, ax
	mov ax, 0A000h ; Set up the graphics segment
	mov gs, ax
	mov ax, 13h ; Move into 320x200x256 mode
	int 10h
	jmp game ; Jump to the game's code
input: ; Sets al to a bitfield containing which of the eight keys on the map are pressed
	pusha ; Preserve registers
	mov ah, [state] ; Restore the state from when this was last run
.read:
	in al, 64h ; Check if the keyboard has sent us any data
	and al, 1
	jnz .done ; Skip if no data is ready
	in al, 60h ; Otherwise, check to see if the key is in the keymap
	mov bh, al
	and bh, 7Fh
	mov bl, al
	and bl, 80h
	mov cl, 1
	mov si, keymap
.scan:
	lodsb
	cmp al, bh
	je .toggle
	shl cl, 1
	cmp cl, 0
	jz .read
	jmp .scan
.toggle:
	cmp bl, 0 ; Determine whether to set or clear the bit
	jz .set
	not cl ; If we've reached this point, we're clearing the bit.
	and ah, cl
	mov [state], ah ; Save the new state
	jmp .read ; Then we check to see if there's another keystroke waiting
.set:
	or ah, cl ; Set the bit
	mov [state], ah ; Save the new state
	jmp .read
.done:
	popa ; Restore registers, set al, and return
	mov al, [state]
	ret
	state db 0 ; The last known input state
delay: ; ax = milliseconds
	pusha ; Preserve registers
	mov bx, 1000 ; Convert milliseconds into something the BIOS understands
	mul bx
	mov cx, dx
	mov dx, ax
	mov ah, 86h ; Tell the BIOS to delay
	int 15h
	popa ; Restore registers and return
	ret
tone: ; ax = frequency
	pusha ; Preserve registers
	movzx ebx, ax ; Convert the frequency into something the PIT can understand
	xor edx, edx
	mov eax, 1193180
	div ebx
	mov cx, ax ; Save the frequency in cx
	mov al, 182 ; Set the PIT frequency
	out 43h, al
	mov ax, cx
	out 42h, al
	mov al, ah
	out 42h, al
	in al, 61h ; Enable the PC speaker
	or al, 3
	out 61h, al
	popa ; Restore registers and return
	ret
mute: ; Works as advertised
	push ax ; Preserve ax
	in al, 61h ; Disable the PC speaker
	and al, 0FCh
	out 61h, al
	pop ax ; Restore ax and return
	ret
clear: ; Works as advertised
	pusha ; Preserve the registers
	xor bx, bx ; Set the graphics offset to zero
.loop:
	mov al, [gs:bx] ; Clear the pixel
	xor al, al
	mov [gs:bx], al
	inc bx ; Move to the next pixel
	cmp bx, 64000 ; Exit the loop if the screen has been completely cleared
	je .done
	jmp .loop ; Otherwise, continue clearing the screen.
.done:
	popa ; Restore the registers and return
	ret
sprite: ; si = sprite, ax = x, bh = y, bl = alpha
	pusha ; Preserve registers
	mov dx, ax ; Move ax to dx for later use
	xor cx, cx ; Set the row(ch) and column numbers(cl) to 0
	mov [alpha], bl ; Save the alpha for later
.line:
	xor eax, eax ; Load the line into eax
	lodsb
	shl eax, 8
	lodsb
	shl eax, 8
	lodsb
	shl eax, 8
.pixel:
	rol eax, 3 ; Load the color into al
	mov bl, [alpha] ; Check and see if the color matches the alpha
	cmp al, bl
	je .skip ; We aren't drawing this pixel if the color matches the alpha
	mov bl, al ; Move the color to bl
	pusha ; Preserve the registers for later
	mov ax, dx ; Move the x coordinate to ax
	add bh, ch ; Calculate the offsets of the initial coordinates
	xor ch, ch
	add ax, cx
	call pixel ; Draw the pixel we calculated
	popa ; Restore the registers from earlier
.skip:
	xor al, al ; Clear the pixel data
	inc cl ; Move to the next pixel
	cmp cl, 8
	je .next
	jmp .pixel
.next:
	xor cl, cl ; Move to the start of the next row of pixels
	inc ch
	cmp ch, 8 ; Unless we're out of pixels to read
	je .done
	jmp .line
.done:
	popa ; Restore registers and return
	ret
	alpha db 0
pixel: ; ax = x, bh = y, bl = color
	pusha ; Preserve registers
;	cmp ax, 320 ; Make sure the pixel will be drawn on the screen
;	jge .done
;	cmp bh, 200
;	jge .done
	mov cx, ax ; Move ax to cx for later use
	push bx ; Save a copy of bh and bl for later
	shr bx, 8 ; Move bh into bl and clear bh
	mov ax, 320 ; Multiply y by the width of the screen
	mul bx
	add ax, cx ; Add x to the offset
	pop bx ; Restore bx
	mov cl, bl ; Mix the 3-bit color with the 5-bit palette number
	and cl, 7
	mov ch, [palette]
	or cl, ch
	mov bx, ax ; Write the pixel to the graphics segment
	mov [gs:bx], cl
.done:
	popa ; Restore registers and return
	ret
set_palette: ; al = palette number
	push ax ; Make sure we preserve the register
	shl al, 3 ; The palette is the high 5 bits of the color
	mov [palette], al
	pop ax ; Restore the state of the register
	ret
	palette db 0
set_keymap: ; si = Keys to map
	pusha ; Preserve registers
	mov cl, 8 ; Import eight keys into the keymap
	mov di, keymap
.loop:
	lodsb
	and al, 7Fh ; Make sure all the keys are in the "off" state
	stosb
	dec cl
	cmp cl, 0 ; Return once we're finished
	je .done
	jmp .loop
.done:
	popa ; Restore registers and return
	ret
	keymap db 17, 30, 31, 32, 22, 35, 36, 37 ; W, A, S, D, U, H, J, K
	times 363-($-$$) db 0
game:
	mov al, 5 ; Set to palette number 5
	call set_palette
	xor ax, ax ; Draw the sprite at (0, 0)
	mov bx, 256
	mov si, test_sprite
	call sprite
	mov ax, 220 ; 220Hz square wave
	call tone
	mov ax, 50 ; 50ms delay
	call delay
	call mute ; Mute the speaker
test_sprite:
	db 00000001b, 10110110b, 11000000b
	db 00000000b, 00110110b, 11011000b
	db 01101101b, 10110110b, 11011011b
	db 01100001b, 10110110b, 11000011b
	db 01100001b, 10110110b, 11000011b
	db 00000001b, 10000000b, 11000000b
	db 00000001b, 10000000b, 11000000b
	db 00001101b, 10000000b, 11011000b
	times 510-($-$$) db 0
	dw 0AA55h
