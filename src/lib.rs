#![feature(lang_items)]
#![feature(const_fn)]
#![feature(unique)]
#![feature(const_unique_new)]
#![feature(ptr_internals)]
#![feature(alloc)]
#![feature(allocator_api)]
#![feature(const_atomic_usize_new)]
#![feature(global_allocator)]
#![no_std]

extern crate rlibc;
extern crate volatile;
extern crate spin;
extern crate multiboot2;
#[macro_use]
extern crate bitflags;
extern crate x86_64;
#[macro_use]
extern crate alloc;
#[macro_use]
extern crate once;
extern crate linked_list_allocator;

#[macro_use]
mod vga_buffer;
mod memory;

use memory::FrameAllocator;
//use memory::BumpAllocator;

use linked_list_allocator::LockedHeap;

// Next: https://os.phil-opp.com/handling-exceptions/
// https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html


// Global allocator
pub const HEAP_START: usize = 0o_000_001_000_000_0000;
pub const HEAP_SIZE: usize = 100 * 1024; // 100KiB

//#[global_allocator]
//static HEAP_ALLOCATOR: BumpAllocator = BumpAllocator::new(HEAP_START, HEAP_START + HEAP_SIZE);

#[global_allocator]
static HEAP_ALLOCATOR: LockedHeap = LockedHeap::empty();



#[no_mangle]
pub extern fn rust_main(multiboot_information_address: usize) {
    // ATTENTION: we have a very small stack and no guard page
    vga_buffer::clear_screen();
    println!("Initializing operational system.");

    let boot_info = unsafe {
        multiboot2::load(multiboot_information_address)
    };
    enable_nx_bit();
    enable_write_protect_bit();

    memory::init(boot_info);
    unsafe {
        HEAP_ALLOCATOR.lock()
            .init(HEAP_START, HEAP_START + HEAP_SIZE);
    }
    
    use alloc::boxed::Box;
    let mut heap_test = Box::new(42);
    *heap_test -= 15;
    let heap_test2 = Box::new("hello");
    println!("{:?} {:?}", heap_test, heap_test2);

    let mut vec_test = vec![1, 2, 3, 4, 5, 6, 7];
    vec_test[3] = 42;
    for i in &vec_test {
        print!("{} ", i);
    }

    println!("\nTesting heap allocation");

    for _ in 0..10000 {
        format!("Some string");
    }

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

