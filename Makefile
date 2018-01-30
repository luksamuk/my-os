arch 	?= x86_64
target 	?= $(arch)-my_os
rust_os := target/$(target)/debug/libmy_os.a
kernel 	:= build/kernel-$(arch).bin
iso	:= build/os-$(arch).iso

linker_script 	:= src/arch/$(arch)/linker.ld
grub_cfg	:= src/arch/$(arch)/grub.cfg
asm_srcs	:= $(wildcard src/arch/$(arch)/*.asm)
asm_objs	:= $(patsubst src/arch/$(arch)/%.asm, \
			build/arch/$(arch)/%.o, $(asm_srcs))

.PHONY: all clean run iso kernel

all: $(kernel)

clean:
	@rm -r build

run: $(iso)
	@qemu-system-x86_64 -cdrom $(iso) -s > /dev/null 2>&1

debug: $(iso)
	@qemu-system-x86_64 -cdrom $(iso) -s -S > /dev/null 2>&1 &
	@gdb "$(kernel)" -ex "target remote :1234"

iso: $(iso)

$(iso): $(kernel) $(grub_cfg)
	@mkdir -p build/isofiles/boot/grub
	@cp $(kernel) build/isofiles/boot/kernel.bin
	@cp $(grub_cfg) build/isofiles/boot/grub
	@grub-mkrescue -o $(iso) build/isofiles 2> /dev/null
	@rm -r build/isofiles

$(kernel): kernel $(rust_os) $(asm_objs) $(linker_script)
	@ld -n --gc-sections -T $(linker_script) -o $(kernel) \
		$(asm_objs) $(rust_os)

# The exported var is a fix for not finding the target specification.
kernel:
	@RUST_TARGET_PATH=`pwd` xargo build --target $(target)

# Compile assembly files
build/arch/$(arch)/%.o: src/arch/$(arch)/%.asm
	@mkdir -p $(shell dirname $@)
	@nasm -f elf64 $< -o $@
