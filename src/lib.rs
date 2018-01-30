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

// Next: https://os.phil-opp.com/remap-the-kernel/
// https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html

#[no_mangle]
pub extern fn rust_main(multiboot_information_address: usize) {
    // ATTENTION: we have a very small stack and no guard page
    vga_buffer::clear_screen();
    
    //println!("Hello, world{}", "!");
    println!("Initializing operational system.");

    // Print memory information
    let boot_info = unsafe { multiboot2::load(multiboot_information_address) };
    let memory_map_tag = boot_info.memory_map_tag()
        .expect("Memory map tag required");

    println!("Memory areas:");
    for area in memory_map_tag.memory_areas() {
        println!("    start: 0x{:x}, length: 0x{:x}",
                 area.base_addr, area.length);
    }


    // Print ELF sections info
    let elf_sections_tag = boot_info.elf_sections_tag()
        .expect("Elf-sections tag required");

    println!("Kernel sections:");
    for section in elf_sections_tag.sections() {
        println!("    addr: 0x{:x}, size: 0x{:x}, flags: 0x{:x}",
                 section.addr, section.size, section.flags);
    }


    // Calculate start and end address of kernel and multiboot
    let kernel_start = elf_sections_tag.sections().map(|s| s.addr)
        .min().unwrap();
    let kernel_end = elf_sections_tag.sections().map(|s| s.addr + s.size)
        .max().unwrap();
    println!("Kernel start: 0x{:x}, Kernel end: 0x{:x}", kernel_start, kernel_end);

    let multiboot_start = multiboot_information_address;
    let multiboot_end = multiboot_start + (boot_info.total_size as usize);
    println!("Multiboot start: 0x{:x}, Multiboot end: 0x{:x}",
             multiboot_start, multiboot_end);

    // Test frame allocator
    let mut frame_allocator = memory::AreaFrameAllocator::new(
        kernel_start as usize, kernel_end as usize,
        multiboot_start, multiboot_end,
        memory_map_tag.memory_areas());

    //println!("Allocated frame: {:?}", frame_allocator.allocate_frame());

    /*for i in 0.. {
        if let None = frame_allocator.allocate_frame() {
            println!("Allocated {} frames", i);
            break;
        }
    }*/
    
    memory::test_paging(&mut frame_allocator);
    
    // Deadlock test
    //println!("{}", { println!("inner"); "outer" });
    //println!("Here is another cute example.");
    //println!("1.0 / 3.0 is {}", 1.0 / 3.0);
    
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
