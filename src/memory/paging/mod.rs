use memory::{PAGE_SIZE, Frame, FrameAllocator};
use core::ops::{Deref, DerefMut};
use multiboot2::BootInformation;
use core::ops::Add;

mod entry;
mod table;
mod temporary_page;
mod mapper;

use self::temporary_page::TemporaryPage;
use self::mapper::Mapper;


pub use self::entry::*;


const ENTRY_COUNT: usize = 512;

pub type PhysicalAddress = usize;
pub type VirtualAddress  = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Page {
    number: usize,
}

impl Page {
    pub fn containing_address(address: VirtualAddress) -> Page {
        assert!(address < 0x0000_80000_0000_0000 ||
                address >= 0xffff_8000_0000_0000,
                "invalid address: 0x{:x}", address);
        Page { number: address / PAGE_SIZE }
    }
    
    pub fn start_address(&self) -> usize {
        self.number * PAGE_SIZE
    }

    
    fn p4_index(&self) -> usize {
        (self.number >> 27) & 0o777
    }
    
    fn p3_index(&self) -> usize {
        (self.number >> 18) & 0o777
    }
    
    fn p2_index(&self) -> usize {
        (self.number >> 9) & 0o777
    }
    
    fn p1_index(&self) -> usize {
        (self.number >> 0) & 0o777
    }

    pub fn range_inclusive(start: Page, end: Page) -> PageIter {
        PageIter {
            start: start,
            end: end,
        }
    }
}


impl Add<usize> for Page {
    type Output = Page;

    fn add(self, rhs: usize) -> Page {
        Page { number: self.number + rhs }
    }
}







#[derive(Clone)]
pub struct PageIter {
    start: Page,
    end: Page,
}

impl Iterator for PageIter {
    type Item = Page;

    fn next(&mut self) -> Option<Page> {
        if self.start <= self.end {
            let page = self.start;
            self.start.number += 1;
            Some(page)
        } else {
            None
        }
    }
}











pub struct ActivePageTable {
    mapper: Mapper,
}

impl Deref for ActivePageTable {
    type Target = Mapper;

    fn deref(&self) -> &Mapper {
        &self.mapper
    }
}

impl DerefMut for ActivePageTable {
    fn deref_mut(&mut self) -> &mut Mapper {
        &mut self.mapper
    }
}

impl ActivePageTable {
    unsafe fn new() -> ActivePageTable {
        ActivePageTable {
            mapper: Mapper::new(),
        }
    }
    
    pub fn with<F>(&mut self, table: &mut InactivePageTable,
                   temporary_page: &mut temporary_page::TemporaryPage, f: F)
        where F: FnOnce(&mut Mapper) {
        use x86_64::instructions::tlb;
        use x86_64::registers::control_regs;

        {
            let backup = Frame::containing_address(control_regs::cr3().0 as usize);

            // Map temporary_page to current p3 table
            let p4_table = temporary_page.map_table_frame(backup.clone(), self);

            // Overwrite recursive mapping
            self.p4_mut()[511].set(table.p4_frame.clone(),
                                   EntryFlags::PRESENT | EntryFlags::WRITABLE);
            tlb::flush_all();

            // Execute f in new context
            f(self);

            // Restore recursive mapping to original P4 table
            p4_table[511].set(backup, EntryFlags::PRESENT | EntryFlags::WRITABLE);
            tlb::flush_all();
        }

        temporary_page.unmap(self);
    }

    pub fn switch(&mut self, new_table: InactivePageTable) -> InactivePageTable {
        use x86_64::PhysicalAddress;
        use x86_64::registers::control_regs;

        let old_table = InactivePageTable {
            p4_frame: Frame::containing_address(control_regs::cr3().0 as usize)
        };

        unsafe {
            control_regs::cr3_write(
                PhysicalAddress(new_table.p4_frame.start_address() as u64));
        }
        old_table
    }
}






pub struct InactivePageTable {
    p4_frame: Frame,
}

impl InactivePageTable {
    pub fn new(frame: Frame, active_table: &mut ActivePageTable,
               temporary_page: &mut TemporaryPage) -> InactivePageTable {
        {
            let table = temporary_page.map_table_frame(frame.clone(), active_table);
            // Zero the table
            table.zero();
            // Setup recursive mapping for table
            table[511].set(frame.clone(), EntryFlags::PRESENT | EntryFlags::WRITABLE);
        }
        temporary_page.unmap(active_table);
        
        InactivePageTable { p4_frame: frame }
    }
}



pub fn remap_the_kernel<A>(allocator: &mut A, boot_info: &BootInformation) -> ActivePageTable
    where A: FrameAllocator {
    let mut temporary_page = TemporaryPage::new(Page {
        number: 0xdeadbeef // yup this is no coincidence
    }, allocator);

    let mut active_table = unsafe { ActivePageTable::new() };
    let mut new_table = {
        let frame = allocator.allocate_frame().expect("no more frames");
        InactivePageTable::new(frame, &mut active_table, &mut temporary_page)
    };

    active_table.with(&mut new_table, &mut temporary_page, |mapper| {
        let elf_sections_tag = boot_info.elf_sections_tag()
            .expect("Memory map tag required");

        // Identity map the allocated kernel sections
        for section in elf_sections_tag.sections() {
            use self::entry::EntryFlags;

            if !section.is_allocated() {
                // Section is not loaded to memory
                continue;
            }

            assert!(section.start_address() % PAGE_SIZE == 0,
                    "sections need to be page-aligned");

            println!("mapping section at addr {:#x}, size: {:#x}",
                     section.addr, section.size);

            let flags = EntryFlags::from_elf_section_flags(section);

            let start_frame = Frame::containing_address(section.start_address());
            let end_frame = Frame::containing_address(section.end_address() - 1);

            for frame in Frame::range_inclusive(start_frame, end_frame) {
                mapper.identity_map(frame, flags, allocator);
            }
        }

        // Identity map the VGA text buffer
        let vga_buffer_frame = Frame::containing_address(0xb8000);
        mapper.identity_map(vga_buffer_frame, EntryFlags::WRITABLE, allocator);

        // Identity map the multiboot info structure
        // We reacquire its info since GRUB can put it anywhere.
        let multiboot_start = Frame::containing_address(boot_info.start_address());
        let multiboot_end = Frame::containing_address(boot_info.end_address() - 1);
        for frame in Frame::range_inclusive(multiboot_start, multiboot_end) {
            mapper.identity_map(frame, EntryFlags::PRESENT, allocator);
        }
    });

    let old_table = active_table.switch(new_table);
    println!("Kernel switched to new page table");


    // Turn the old P4 page into a Guard Page
    let old_p4_page = Page::containing_address(old_table.p4_frame.start_address());
    active_table.unmap(old_p4_page, allocator);
    println!("Guard page at {:#x}", old_p4_page.start_address());

    active_table
}













// TEST
/*pub fn test_paging<A>(allocator: &mut A) where A: FrameAllocator {
    let mut page_table = unsafe { ActivePageTable::new() };

    // Test it!
    println!("Testing frame allocation");
    let addr = 42 * 512 * 512 * 4096; // 42th P3 entry
    let page = Page::containing_address(addr);
    let frame = allocator.allocate_frame()
        .expect("no more frames");

    println!("None = {:?}, map to {:?}",
             page_table.translate(addr),
             frame);
    page_table.map_to(page, frame, EntryFlags::empty(), allocator);
    println!("Some = {:?}", page_table.translate(addr));
    println!("next free frame: {:?}", allocator.allocate_frame());

    println!("{:#x}", unsafe {
        *(Page::containing_address(addr).start_address() as *const u64)
    });

    println!("Testing page unmapping");
    page_table.unmap(Page::containing_address(addr), allocator);
    println!("None = {:?}", page_table.translate(addr));


    // This re-reading after an unmap will trigger a page fault.
    // Uncomment this for tests at your own risk.
    //println!("{:#x}", unsafe {
    //    *(Page::containing_address(addr).start_address() as *const u64)
    //});
}*/
