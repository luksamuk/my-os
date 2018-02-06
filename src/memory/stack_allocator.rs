use memory::paging::{self, PageIter, Page, ActivePageTable};
use memory::{PAGE_SIZE, FrameAllocator};

pub struct StackAllocator {
    range: PageIter,
}

impl StackAllocator {
    pub fn new(page_range: PageIter) -> StackAllocator {
        StackAllocator { range: page_range }
    }

    pub fn alloc_stack<FA: FrameAllocator>(&mut self,
                                           active_table: &mut ActivePageTable,
                                           frame_allocator: &mut FA,
                                           size_in_pages: usize) -> Option<Stack> {
        if size_in_pages == 0 {
            return None; // 0-size stack makes no sense
        }

        // Clone range so it only changes on success
        let mut range = self.range.clone();

        // Try allocating stack pages and guard page
        let guard_page = range.next();
        let stack_start = range.next();
        let stack_end = if size_in_pages == 1 {
            stack_start
        } else {
            // Chooses (size_in_pages - 2)th element.
            // Index starts at 0, and we already allocated
            // the start page
            range.nth(size_in_pages - 2)
        };

        match (guard_page, stack_start, stack_end) {
            (Some(_), Some(start), Some(end)) => {
                // Everything ok, write back updated range
                self.range = range;

                // Map stack pages to physical frames
                for page in Page::range_inclusive(start, end) {
                    active_table.map(page, paging::EntryFlags::WRITABLE, frame_allocator);
                }

                // Create new stack
                let top_of_stack = end.start_address() + PAGE_SIZE;
                Some(Stack::new(top_of_stack, start.start_address()))
            }
            _ => None, // Not enough pages
        }
    }
}


#[derive(Debug)]
pub struct Stack {
    top: usize,
    bottom: usize,
}

impl Stack {
    fn new(top: usize, bottom: usize) -> Stack {
        assert!(top > bottom);
        Stack {
            top: top,
            bottom: bottom,
        }
    }

    pub fn top(&self) -> usize {
        self.top
    }

    pub fn bottom(&self) -> usize {
        self.bottom
    }
}
