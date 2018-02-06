#![feature(lang_items)]
#![feature(const_fn)]
#![feature(unique)]
#![feature(const_unique_new)]
#![feature(ptr_internals)]
#![feature(alloc)]
#![feature(allocator_api)]
#![feature(const_atomic_usize_new)]
#![feature(global_allocator)]
#![feature(abi_x86_interrupt)]
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
extern crate lazy_static;
extern crate bit_field;

#[macro_use]
mod vga_buffer;
mod memory;
mod interrupts;

//use memory::BumpAllocator;
use linked_list_allocator::LockedHeap;

// Next: https://os.phil-opp.com/double-faults/  -- Halted! Awaiting new post!


// Global allocator
pub const HEAP_START: usize = 0o_000_001_000_000_0000;
//pub const HEAP_SIZE: usize = 100 * 1024; // 100KB
pub const HEAP_SIZE: usize = 1000 * 1024; // 1MB

//#[global_allocator]
//static HEAP_ALLOCATOR: BumpAllocator = BumpAllocator::new(HEAP_START, HEAP_START + HEAP_SIZE);

#[global_allocator]
static HEAP_ALLOCATOR: LockedHeap = LockedHeap::empty();



#[no_mangle]
pub extern fn rust_main(multiboot_information_address: usize) {
    vga_buffer::clear_screen();
    println!("Initializing operational system.");

    // Security and boot related
    let boot_info = unsafe {
        multiboot2::load(multiboot_information_address)
    };
    enable_nx_bit();
    enable_write_protect_bit();

    // Initialize memory
    let mut memory_controller = memory::init(boot_info);
    
    unsafe {
        HEAP_ALLOCATOR.lock()
            .init(HEAP_START, HEAP_START + HEAP_SIZE);
    }

    // Initialize interrupt handling
    interrupts::init(&mut memory_controller);


    println!("\nTesting heap allocation");
    println!("WARNING: We have very small stack and heap");

    {   
        // Boxed values
        println!("Boxed value test");
        {
            use alloc::boxed::Box;
            
            let mut heap_test = Box::new(42);
            *heap_test -= 15;
            let _heap_test2 = Box::new("hello"); 
        }

        // Basic vectors
        println!("Basic dynamic vector test");
        {
            let mut vec_test = vec![1, 2, 3, 4, 5, 6, 7];
            vec_test[3] = 42;
        }
        
        // Heavy allocation
        const NUM_ITERATIONS: usize = 15000;
        const NUM_TESTS: usize = 10;
        for i in 0..NUM_TESTS {
            println!("Heavy allocation {}/{} test with {} iterations",
                     i + 1, NUM_TESTS, NUM_ITERATIONS);
            let mut string_vector = alloc::Vec::new();
            for _ in 0..NUM_ITERATIONS {
                string_vector.push(format!("Some string"));
            }
        }
    }

    println!("Operating system did not crash in any way! Congratulations!");


    println!("\nEverything gone smoothly. System will now purposely hang.");

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

