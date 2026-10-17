//! The allocator of the `blots` binary: pools for small blocks, the system allocator for the
//! rest, plus a hint for large blocks.
//!
//! An interpreter allocates small blocks at a furious rate (the nodes of a syntax tree, the
//! code of a statement, every list, string and closure a program creates) and gives most of
//! them back almost at once. A general-purpose allocator spends a few hundred instructions on
//! each round trip once its shortcuts overflow, which is more than the interpreter spends on
//! whatever it wanted the block for. So blocks of up to [`pool::MAX_SIZE`] bytes are served
//! from per-thread free lists, one for each size, where taking or returning a block is a
//! handful of instructions. See [`pool`] for what that costs.
//!
//! A program that builds a list of a few million elements writes to tens of megabytes of
//! memory the process has never touched, and with 4 KiB pages the kernel is asked for every
//! one of them separately; those page faults can take longer than computing the elements. On
//! Linux, blocks that are large enough are therefore marked as candidates for transparent
//! huge pages, which are faulted in 2 MiB at a time. (Where the system's huge page policy is
//! "always" that happens anyway; the more common "madvise" policy leaves it to programs to
//! ask.)

use std::alloc::{GlobalAlloc, Layout, System};
use std::ptr;

pub struct Allocator;

// SAFETY: a block comes either from a pool or from the system allocator, and which of the two
// is a function of its layout alone, so every block goes back to where it came from. Pooled
// blocks are aligned to `pool::GRANULE`, which is all a pooled layout may ask for. The hint
// does not alter the content or the validity of a block.
unsafe impl GlobalAlloc for Allocator {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if pool::takes(layout.size(), layout.align()) {
            // SAFETY: the pools take this layout.
            return unsafe { pool::alloc(layout.size()) };
        }

        // SAFETY: the caller upholds the contract of `GlobalAlloc::alloc`.
        let block = unsafe { System.alloc(layout) };
        huge_pages::suggest(block, layout.size());

        block
    }

    #[inline]
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        if pool::takes(layout.size(), layout.align()) {
            // SAFETY: the pools take this layout.
            let block = unsafe { pool::alloc(layout.size()) };
            if !block.is_null() {
                // SAFETY: the block is at least as long as the layout.
                unsafe { ptr::write_bytes(block, 0, layout.size()) };
            }

            return block;
        }

        // SAFETY: the caller upholds the contract of `GlobalAlloc::alloc_zeroed`.
        let block = unsafe { System.alloc_zeroed(layout) };
        huge_pages::suggest(block, layout.size());

        block
    }

    #[inline]
    unsafe fn dealloc(&self, block: *mut u8, layout: Layout) {
        if pool::takes(layout.size(), layout.align()) {
            // SAFETY: a block of a layout the pools take came from them.
            return unsafe { pool::dealloc(block, layout.size()) };
        }

        // SAFETY: the caller upholds the contract of `GlobalAlloc::dealloc`.
        unsafe { System.dealloc(block, layout) }
    }

    #[inline]
    unsafe fn realloc(&self, block: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let was_pooled = pool::takes(layout.size(), layout.align());
        let is_pooled = pool::takes(new_size, layout.align());

        if !was_pooled && !is_pooled {
            // SAFETY: the caller upholds the contract of `GlobalAlloc::realloc`.
            let block = unsafe { System.realloc(block, layout, new_size) };
            huge_pages::suggest(block, new_size);

            return block;
        }

        if was_pooled && is_pooled && pool::same_block(layout.size(), new_size) {
            return block;
        }

        // SAFETY: the caller guarantees that `new_size` makes a valid layout with the
        // alignment of the old one; the two blocks are distinct, and each is at least as long
        // as the smaller of the two sizes.
        unsafe {
            let new_layout = Layout::from_size_align_unchecked(new_size, layout.align());
            let new_block = self.alloc(new_layout);
            if !new_block.is_null() {
                ptr::copy_nonoverlapping(block, new_block, layout.size().min(new_size));
                self.dealloc(block, layout);
            }

            new_block
        }
    }
}

/// Free lists of small blocks.
///
/// Every size that is a multiple of [`GRANULE`] up to [`MAX_SIZE`] has a list of its own. A
/// block that is given back goes to the front of the list of its size and is the first to be
/// handed out again; when a list is empty, a block is cut from a chunk obtained from the
/// system allocator. Blocks carry no header: Rust says how large a block is when it returns
/// it.
///
/// The simplicity has a price. A block never changes size, and chunks are never returned to
/// the system, so memory freed at one size cannot serve requests of another: a program that
/// frees a great many small blocks of one size and then allocates as many of a different size
/// uses the sum of the two. Blocks above [`MAX_SIZE`], which is where most of the memory of
/// such a program would be, are not affected.
///
/// Each thread has its own lists, which is what makes them cheap. A block may be returned on
/// another thread than the one it was allocated on; it then joins that thread's lists. The
/// lists of a thread that ends are not reclaimed.
mod pool {
    use super::huge_pages;
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::cell::UnsafeCell;
    use std::ptr;

    /// Block sizes are multiples of this, and blocks are aligned to it.
    pub const GRANULE: usize = 16;

    /// The largest block that comes from the pools.
    pub const MAX_SIZE: usize = 512;

    const CLASSES: usize = MAX_SIZE / GRANULE;

    /// The size of the first chunk a thread gets from the system; each one after it is twice
    /// as large as the one before, up to [`MAX_CHUNK`]. Pages of a chunk that are never
    /// written to are never backed by memory.
    const FIRST_CHUNK: usize = 64 << 10;

    const MAX_CHUNK: usize = 4 << 20;

    struct Pools {
        /// For each size class, the block that was freed last. The first word of a free block
        /// holds the address of the one freed before it.
        free: [*mut u8; CLASSES],

        /// The part of the current chunk that has not been handed out.
        fresh: *mut u8,
        fresh_end: *mut u8,

        next_chunk: usize,
    }

    thread_local! {
        // A constant initialiser and nothing to drop: reaching the pools takes no check and
        // never allocates.
        static POOLS: UnsafeCell<Pools> = const {
            UnsafeCell::new(Pools {
                free: [ptr::null_mut(); CLASSES],
                fresh: ptr::null_mut(),
                fresh_end: ptr::null_mut(),
                next_chunk: FIRST_CHUNK,
            })
        };
    }

    /// The size class of blocks that hold `size` bytes. A size of zero has none: its class is
    /// out of range.
    #[inline(always)]
    fn class(size: usize) -> usize {
        size.wrapping_sub(1) / GRANULE
    }

    /// Whether blocks of this layout come from the pools.
    #[inline(always)]
    pub fn takes(size: usize, align: usize) -> bool {
        class(size) < CLASSES && align <= GRANULE
    }

    /// Whether a block that holds `size` bytes is exactly as long as one that holds
    /// `other_size`.
    #[inline(always)]
    pub fn same_block(size: usize, other_size: usize) -> bool {
        class(size) == class(other_size)
    }

    /// # Safety
    /// `takes(size, align)` must hold for the alignment the caller needs.
    #[inline(always)]
    pub unsafe fn alloc(size: usize) -> *mut u8 {
        POOLS.with(|pools| {
            // SAFETY: nothing in this module runs while another reference to the pools of the
            // thread exists, and free blocks hold the address of the next one (or null).
            unsafe {
                let pools = &mut *pools.get();
                let class = class(size);

                let block = *pools.free.get_unchecked(class);
                if !block.is_null() {
                    *pools.free.get_unchecked_mut(class) = block.cast::<*mut u8>().read();

                    return block;
                }

                pools.cut((class + 1) * GRANULE)
            }
        })
    }

    /// # Safety
    /// `block` must have come from [`alloc`] with a size of the same class as `size`, and
    /// must not be used again.
    #[inline(always)]
    pub unsafe fn dealloc(block: *mut u8, size: usize) {
        POOLS.with(|pools| {
            // SAFETY: as in `alloc`; the block is at least a granule long and aligned to one,
            // so there is room for an address in it.
            unsafe {
                let pools = &mut *pools.get();
                let head = pools.free.get_unchecked_mut(class(size));

                block.cast::<*mut u8>().write(*head);
                *head = block;
            }
        })
    }

    impl Pools {
        /// Cut a block of `bytes` bytes, a multiple of the granule, from the current chunk.
        #[inline(always)]
        unsafe fn cut(&mut self, bytes: usize) -> *mut u8 {
            if (self.fresh_end as usize) - (self.fresh as usize) < bytes {
                // SAFETY: guaranteed by the caller.
                return unsafe { self.cut_from_new_chunk(bytes) };
            }

            let block = self.fresh;
            // SAFETY: the chunk has at least `bytes` bytes left.
            self.fresh = unsafe { block.add(bytes) };

            block
        }

        #[cold]
        #[inline(never)]
        unsafe fn cut_from_new_chunk(&mut self, bytes: usize) -> *mut u8 {
            let size = self.next_chunk;
            // SAFETY: chunk sizes are non-zero multiples of the granule, a power of two.
            let chunk = unsafe { System.alloc(Layout::from_size_align_unchecked(size, GRANULE)) };
            if chunk.is_null() {
                return chunk;
            }

            huge_pages::suggest(chunk, size);

            // What is left of the old chunk is too short for this block, but it is a whole
            // number of granules: a block of a smaller size.
            let rest = (self.fresh_end as usize) - (self.fresh as usize);
            if rest != 0 {
                // SAFETY: the rest of the chunk is unused, and `rest` is the size of a class.
                unsafe {
                    let head = self.free.get_unchecked_mut(class(rest));

                    self.fresh.cast::<*mut u8>().write(*head);
                    *head = self.fresh;
                }
            }

            self.next_chunk = (size * 2).min(MAX_CHUNK);

            // SAFETY: a chunk is longer than any block.
            unsafe {
                self.fresh = chunk.add(bytes);
                self.fresh_end = chunk.add(size);
            }

            chunk
        }
    }
}

#[cfg(target_os = "linux")]
mod huge_pages {
    use std::ffi::c_void;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// The size of a transparent huge page on the common configurations; smaller blocks
    /// cannot contain one.
    const HUGE_PAGE: usize = 2 << 20;

    /// The page size of the system, once it has been asked for.
    static PAGE_SIZE: AtomicUsize = AtomicUsize::new(0);

    /// Tell the kernel that the memory of a freshly allocated block may be backed by huge
    /// pages, if the block is large enough for that to matter.
    #[inline]
    pub fn suggest(block: *mut u8, size: usize) {
        if size >= HUGE_PAGE && !block.is_null() {
            suggest_range(block, size);
        }
    }

    #[cold]
    fn suggest_range(block: *mut u8, size: usize) {
        let page_size = match PAGE_SIZE.load(Ordering::Relaxed) {
            0 => {
                // SAFETY: `sysconf` only reads its argument.
                let page_size =
                    usize::try_from(unsafe { libc::sysconf(libc::_SC_PAGESIZE) }).unwrap_or(0);
                if !page_size.is_power_of_two() {
                    return;
                }

                PAGE_SIZE.store(page_size, Ordering::Relaxed);
                page_size
            }
            page_size => page_size,
        };

        // The hint covers every page the block touches. A block this large has its pages
        // (almost) to itself, and a hint that also reaches a neighbour does no harm.
        let start = block as usize & !(page_size - 1);
        let Some(end) = (block as usize)
            .checked_add(size)
            .and_then(|end| end.checked_next_multiple_of(page_size))
        else {
            return;
        };

        // SAFETY: the advice changes how the kernel backs the pages, not what they hold. It
        // is only a hint: failure (for instance on a kernel without huge page support)
        // leaves everything as it was.
        unsafe { libc::madvise(start as *mut c_void, end - start, libc::MADV_HUGEPAGE) };
    }
}

#[cfg(not(target_os = "linux"))]
mod huge_pages {
    #[inline]
    pub fn suggest(_block: *mut u8, _size: usize) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A small deterministic generator, so the tests need no dependency and no seed.
    struct Random(u64);

    impl Random {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;

            self.0
        }

        fn below(&mut self, bound: usize) -> usize {
            (self.next() % bound as u64) as usize
        }

        /// A size that is usually small and often right at the edge of a size class.
        fn size(&mut self) -> usize {
            match self.below(8) {
                0 => 1 + self.below(4096),
                1 => pool::GRANULE * (1 + self.below(40)),
                2 => pool::GRANULE * (1 + self.below(40)) + 1,
                3 => pool::MAX_SIZE - 1 + self.below(3),
                _ => 1 + self.below(600),
            }
        }
    }

    /// A live block and the byte each of its positions should hold.
    struct Block {
        start: *mut u8,
        layout: Layout,
        seed: u8,
    }

    impl Block {
        fn expected(&self, position: usize) -> u8 {
            self.seed.wrapping_add(position as u8).wrapping_mul(31)
        }

        fn fill(&self, from: usize) {
            for position in from..self.layout.size() {
                // SAFETY: the position lies within the block.
                unsafe { self.start.add(position).write(self.expected(position)) };
            }
        }

        fn check(&self, len: usize) {
            for position in 0..len {
                // SAFETY: the position lies within the block.
                let byte = unsafe { self.start.add(position).read() };
                assert_eq!(
                    byte,
                    self.expected(position),
                    "byte {} of a block",
                    position
                );
            }
        }
    }

    /// Allocate, resize and free blocks at random, checking that each one keeps its content
    /// (and so shares no memory with any other) and is aligned as asked.
    fn churn(seed: u64, rounds: usize) {
        let mut random = Random(seed);
        let mut blocks: Vec<Block> = Vec::new();

        for round in 0..rounds {
            match random.below(if blocks.len() > 400 { 6 } else { 4 }) {
                0 | 1 => {
                    let align = 1 << random.below(7);
                    let layout = Layout::from_size_align(random.size(), align).unwrap();
                    let zeroed = random.below(4) == 0;

                    // SAFETY: the layout has a non-zero size.
                    let start = unsafe {
                        if zeroed {
                            Allocator.alloc_zeroed(layout)
                        } else {
                            Allocator.alloc(layout)
                        }
                    };
                    assert!(!start.is_null());
                    assert_eq!(start as usize % align, 0);

                    if zeroed {
                        for position in 0..layout.size() {
                            // SAFETY: the position lies within the block.
                            assert_eq!(unsafe { start.add(position).read() }, 0);
                        }
                    }

                    let block = Block {
                        start,
                        layout,
                        seed: round as u8,
                    };
                    block.fill(0);
                    blocks.push(block);
                }
                2 if !blocks.is_empty() => {
                    let index = random.below(blocks.len());
                    let block = &mut blocks[index];
                    let old_size = block.layout.size();
                    let new_size = random.size();

                    // SAFETY: the block is live and was allocated with this layout.
                    let start = unsafe { Allocator.realloc(block.start, block.layout, new_size) };
                    assert!(!start.is_null());
                    assert_eq!(start as usize % block.layout.align(), 0);

                    block.start = start;
                    block.layout = Layout::from_size_align(new_size, block.layout.align()).unwrap();
                    block.check(old_size.min(new_size));
                    block.fill(old_size.min(new_size));
                }
                _ if !blocks.is_empty() => {
                    let block = blocks.swap_remove(random.below(blocks.len()));
                    block.check(block.layout.size());

                    // SAFETY: the block is live and was allocated with this layout.
                    unsafe { Allocator.dealloc(block.start, block.layout) };
                }
                _ => {}
            }
        }

        for block in blocks {
            block.check(block.layout.size());

            // SAFETY: the block is live and was allocated with this layout.
            unsafe { Allocator.dealloc(block.start, block.layout) };
        }
    }

    #[test]
    fn blocks_keep_their_content() {
        churn(0x9E37_79B9_7F4A_7C15, 200_000);
    }

    #[test]
    fn threads_have_pools_of_their_own() {
        let threads: Vec<_> = (1..=4u64)
            .map(|thread| std::thread::spawn(move || churn(thread * 0x2545_F491_4F6C_DD1D, 50_000)))
            .collect();

        for thread in threads {
            thread.join().unwrap();
        }
    }

    #[test]
    fn a_block_may_be_freed_on_another_thread() {
        let boxes: Vec<Box<[u64; 5]>> = (0..10_000).map(|value| Box::new([value; 5])).collect();

        let sum = std::thread::spawn(move || boxes.iter().map(|values| values[4]).sum::<u64>())
            .join()
            .unwrap();
        assert_eq!(sum, 9_999 * 10_000 / 2);

        // Blocks of that size are still to be had here, and are distinct.
        let again: Vec<Box<[u64; 5]>> = (0..10_000).map(|value| Box::new([value; 5])).collect();
        assert!(
            again
                .iter()
                .enumerate()
                .all(|(value, values)| values[0] == value as u64)
        );
    }

    #[test]
    fn a_freed_block_is_the_next_of_its_size() {
        let layout = Layout::from_size_align(40, 8).unwrap();

        // SAFETY: the layout has a non-zero size, and each block is freed once, with it.
        unsafe {
            let first = Allocator.alloc(layout);
            let second = Allocator.alloc(layout);
            assert_ne!(first, second);

            Allocator.dealloc(first, layout);
            assert_eq!(
                Allocator.alloc(Layout::from_size_align(33, 1).unwrap()),
                first
            );

            Allocator.dealloc(first, layout);
            Allocator.dealloc(second, layout);
        }
    }

    #[test]
    fn growing_within_a_block_does_not_move_it() {
        let layout = Layout::from_size_align(17, 1).unwrap();

        // SAFETY: the layout has a non-zero size; the block is resized and freed with the
        // layout it has at the time.
        unsafe {
            let block = Allocator.alloc(layout);
            assert_eq!(Allocator.realloc(block, layout, 32), block);

            let grown = Layout::from_size_align(32, 1).unwrap();
            let moved = Allocator.realloc(block, grown, 33);
            assert_ne!(moved, block);

            Allocator.dealloc(moved, Layout::from_size_align(33, 1).unwrap());
        }
    }
}
