section .multiboot_header
header_start:
	dd 0xe85250d6 		     ; Magic number (Multiboot 2)
	dd 0			     ; For i386, Protected Mode
	dd header_end - header_start ; Header length

	; Checksum
	dd 0x100000000 - (0xe85250d6 + 0 + (header_end - header_start))

	; TODO: Optional Multiboot tags

	; End tag
	dw 0 			; type
	dw 0 			; flags
	dd 8 			; size
header_end:
	
