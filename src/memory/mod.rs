mod area_frame_allocator;
mod paging;
mod heap_allocator;


pub use self::area_frame_allocator::AreaFrameAllocator;
pub use self::paging::remap_the_kernel;
pub use self::heap_allocator::BumpAllocator;

//pub use self::paging::test_paging;


use self::paging::PhysicalAddress;
use multiboot2::BootInformation;

pub const PAGE_SIZE: usize = 4096;

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct Frame {
    number: usize,
}

impl Frame {
    fn containing_address(address: usize) -> Frame {
        Frame { number: address / PAGE_SIZE }
    }

    fn start_address(&self) -> PhysicalAddress {
        self.number * PAGE_SIZE
    }

    fn clone(&self) -> Frame {
        Frame { number: self.number }
    }

    fn range_inclusive(start: Frame, end: Frame) -> FrameIter {
        FrameIter {
            start: start,
            end: end,
        }
    }
}



struct FrameIter {
    start: Frame,
    end: Frame,
}

impl Iterator for FrameIter {
    type Item = Frame;

    fn next(&mut self) -> Option<Frame> {
        if self.start <= self.end {
            let frame = self.start.clone();
            self.start.number += 1;
            Some(frame)
        } else {
            None
        }
    }
}




pub trait FrameAllocator {
    fn allocate_frame(&mut self) -> Option<Frame>;
    fn deallocate_frame(&mut self, frame: Frame);
}




/// Converts a raw number of bytes to kilobytes (KB).
/// Note that the value is roughly truncated.
pub fn to_kilobytes(bytes_size: usize) -> usize {
    bytes_size / (1024 as usize)
}



pub fn init(boot_info: &BootInformation) {
    assert_has_not_been_called!("memory::init must only be called once");
    
    let memory_map_tag = boot_info.memory_map_tag()
        .expect("Memory map tag required");
    let elf_sections_tag = boot_info.elf_sections_tag()
        .expect("Elf sections tag required");

    let kernel_start = elf_sections_tag.sections()
        .filter(|s| s.is_allocated())
        .map(|s| s.addr)
        .min().unwrap();
    let kernel_end = elf_sections_tag.sections()
        .filter(|s| s.is_allocated())
        .map(|s| s.addr + s.size)
        .max().unwrap();
    let multiboot_start = boot_info.start_address();
    let multiboot_end = multiboot_start + (boot_info.total_size as usize);

    println!("Kernel start: {:#x}, Kernel end: {:#x} ({} KB)",
             kernel_start, kernel_end,
             to_kilobytes((kernel_end - kernel_start) as usize));
    println!("Multiboot start: {:#x}, Multiboot end: {:#x} ({} KB)",
             multiboot_start, multiboot_end,
             to_kilobytes(boot_info.total_size as usize));

    let mut frame_allocator = AreaFrameAllocator::new(
        kernel_start as usize, kernel_end as usize,
        multiboot_start, multiboot_end,
        memory_map_tag.memory_areas());

    let mut active_table =
        paging::remap_the_kernel(&mut frame_allocator, boot_info);

    use self::paging::Page;
    use {HEAP_START, HEAP_SIZE};

    let heap_start_page = Page::containing_address(HEAP_START);
    let heap_end_page = Page::containing_address(HEAP_START + HEAP_SIZE - 1);

    for page in Page::range_inclusive(heap_start_page, heap_end_page) {
        active_table.map(page, paging::EntryFlags::WRITABLE, &mut frame_allocator);
    }

    println!("Heap start: {:#x}, Heap end: {:#x} ({} KB)",
             HEAP_START, HEAP_START + HEAP_SIZE,
             to_kilobytes(HEAP_SIZE));
}
