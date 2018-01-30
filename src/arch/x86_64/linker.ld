ENTRY(start)

SECTIONS {
	 . = 1M;
	 .boot :
	 {
		/* ensure that the multiboot header is at beginning */
		KEEP(*(.multiboot_header))
	 }

	 /* ensure that we merge subsections */
	 .text :
	 {
		*(.text .text.*)
	 }

	 .rodata : {
	 	 *(.rodata .rodata.*)
	 }
	 
	 .data.rel.ro : {
	 	 *(.data.rel.ro.local*) *(.data.rel.ro .data.rel.ro.*)
	 }
}
