#![feature(lang_items)]
#![feature(const_fn)]
#![feature(unique)]
#![feature(const_unique_new)]
#![feature(ptr_internals)]
#![no_std]

extern crate rlibc;
extern crate volatile;
extern crate spin;
extern crate multiboot2;
#[macro_use]
extern crate bitflags;
extern crate x86_64;

#[macro_use]
mod vga_buffer;
mod memory;

use memory::FrameAllocator;

// Next: https://os.phil-opp.com/kernel-heap/
// https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html



#[no_mangle]
pub extern fn rust_main(multiboot_information_address: usize) {
    // ATTENTION: we have a very small stack and no guard page
    vga_buffer::clear_screen();
    
    println!("Initializing operational system.");

    // Print memory information
    let boot_info = unsafe { multiboot2::load(multiboot_information_address) };
    let memory_map_tag = boot_info.memory_map_tag()
        .expect("Memory map tag required");
    let elf_sections_tag = boot_info.elf_sections_tag()
        .expect("Elf-sections tag required");

    // Print memory areas info
    /*println!("Memory areas:");
    for area in memory_map_tag.memory_areas() {
        println!("    start: 0x{:x}, length: 0x{:x}",
                 area.base_addr, area.length);
    }


    // Print ELF sections info
    println!("Kernel sections:");
    for section in elf_sections_tag.sections() {
        println!("    addr: 0x{:x}, size: 0x{:x}, flags: 0x{:x}",
                 section.addr, section.size, section.flags);
    }*/


    // Calculate start and end address of kernel and multiboot
    let kernel_start = elf_sections_tag.sections().map(|s| s.addr)
        .min().unwrap();
    let kernel_end = elf_sections_tag.sections().map(|s| s.addr + s.size)
        .max().unwrap();
    let multiboot_start = multiboot_information_address;
    let multiboot_end = multiboot_start + (boot_info.total_size as usize);

    
    println!("Kernel start: 0x{:x}, Kernel end: 0x{:x}", kernel_start, kernel_end);
    println!("Multiboot start: 0x{:x}, Multiboot end: 0x{:x}",
             multiboot_start, multiboot_end);

    // Instantiate frame allocator
    let mut frame_allocator = memory::AreaFrameAllocator::new(
        kernel_start as usize, kernel_end as usize,
        multiboot_start, multiboot_end,
        memory_map_tag.memory_areas());

    // Turn on NX bit
    enable_nx_bit();
    
    // Turn on write protection for kernel code
    enable_write_protect_bit();

    // Remap kernel
    println!("Remapping kernel");
    memory::remap_the_kernel(&mut frame_allocator, boot_info);
    frame_allocator.allocate_frame();
    println!("Well well well, looks like it didn't crash.");



    // Here we test some features.
    //vga_buffer::print_something();
    //memory::test_paging(&mut frame_allocator);
    // also try uncommenting memory::paging::test_tables

    // Loop forever
    loop {}
}

#[lang = "eh_personality"]
#[no_mangle]
pub extern fn eh_personality() {}

#[lang = "panic_fmt"]
#[no_mangle]
pub extern fn panic_fmt(fmt: core::fmt::Arguments, file: &'static str, line: u32) -> ! {
    println!("\n\nPANIC in {} at line {}:", file, line);
    println!("    {}", fmt);
    loop {}
}




fn enable_nx_bit() {
    // Does it really work on Intel or only for AMD?
    use x86_64::registers::msr::{IA32_EFER, rdmsr, wrmsr};

    let nx_bit = 1 << 11;
    unsafe {
        let efer = rdmsr(IA32_EFER);
        wrmsr(IA32_EFER, efer | nx_bit);
    }
}

fn enable_write_protect_bit() {
    use x86_64::registers::control_regs::{cr0, cr0_write, Cr0};

    unsafe { cr0_write(cr0() | Cr0::WRITE_PROTECT) };
}

