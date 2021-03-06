use memory::paging::entry::*;
use memory::paging::ENTRY_COUNT;
use core::ops::{Index, IndexMut};
use core::marker::PhantomData;
use memory::FrameAllocator;

// Table Levels
pub trait TableLevel {}

pub enum Level4 {}
pub enum Level3 {}
pub enum Level2 {}
pub enum Level1 {}

impl TableLevel for Level4 {}
impl TableLevel for Level3 {}
impl TableLevel for Level2 {}
impl TableLevel for Level1 {}


// Hierarchical levels to guarantee we don't access
// a next table address on P1.
// We just do type checking magic so we don't have
// to check this at runtime
pub trait HierarchicalLevel: TableLevel {
    type NextLevel: TableLevel;
}

impl HierarchicalLevel for Level4 {
    type NextLevel = Level3;
}
impl HierarchicalLevel for Level3 {
    type NextLevel = Level2;
}
impl HierarchicalLevel for Level2 {
    type NextLevel = Level1;
}





pub struct Table<L: TableLevel> {
    entries: [Entry; ENTRY_COUNT],
    level: PhantomData<L>,
}

impl<L> Index<usize> for Table<L> where L: TableLevel {
    type Output = Entry;

    fn index(&self, index: usize) -> &Entry {
        &self.entries[index]
    }
}

impl<L> IndexMut<usize> for Table<L> where L: TableLevel {
    fn index_mut(&mut self, index: usize) -> &mut Entry {
        &mut self.entries[index]
    }
}

impl<L> Table<L> where L: TableLevel {
    pub fn zero(&mut self) {
        for entry in self.entries.iter_mut() {
            entry.set_unused();
        }
    }
}

impl<L> Table<L> where L: HierarchicalLevel {
    fn next_table_address(&self, index: usize) -> Option<usize> {
        let entry_flags = self[index].flags();
        if entry_flags.contains(EntryFlags::PRESENT)
            && !entry_flags.contains(EntryFlags::HUGE_PAGE) {
            let table_address = self as *const _  as usize;
            Some((table_address << 9) | (index << 12))
        } else {
            None
        }
    }

    pub fn next_table(&self, index: usize) -> Option<&Table<L::NextLevel>> {
        self.next_table_address(index)
            .map(|address| unsafe { &*(address as *const _) })
    }

    pub fn next_table_mut(&mut self, index: usize) -> Option<&mut Table<L::NextLevel>> {
        self.next_table_address(index)
            .map(|address| unsafe { &mut *(address as *mut _) })
    }

    

    pub fn next_table_create<A>(&mut self, index: usize, allocator: &mut A)
                                -> &mut Table<L::NextLevel>
        where A: FrameAllocator {
        if self.next_table(index).is_none() {
            assert!(!self.entries[index].flags().contains(EntryFlags::HUGE_PAGE),
                    "mapping code does not support huge pages");
            let frame = allocator.allocate_frame()
                .expect("no frames available");
            self.entries[index].set(frame, EntryFlags::PRESENT | EntryFlags::WRITABLE);
            self.next_table_mut(index).unwrap().zero();
        }
        self.next_table_mut(index).unwrap()
    }
}



// Reference to page 4
pub const P4: *mut Table<Level4> = 0xffffffff_fffff000 as *mut _;



// TEST
// NOTE: IT FAILS AT COMPILE TIME!
/*pub fn test_tables() {
    let p4 = unsafe { &*P4 };
    // The following addresses are mostly invalid,
    // but hopefully this fails at compile time,
    // because we can't call a next_page on level 1
    p4.next_table(42)
        .and_then(|p3| p3.next_table(1337))
        .and_then(|p2| p2.next_table(0xdeadbeaf))
        .and_then(|p1| p1.next_table(0xcafebabe))
}*/
