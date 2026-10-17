//! The allocator of the `blots` binary: the system allocator, plus a hint for large blocks.
//!
//! A program that builds a list of a few million elements writes to tens of megabytes of
//! memory the process has never touched, and with 4 KiB pages the kernel is asked for every
//! one of them separately; those page faults can take longer than computing the elements. On
//! Linux, blocks that are large enough are therefore marked as candidates for transparent
//! huge pages, which are faulted in 2 MiB at a time. (Where the system's huge page policy is
//! "always" that happens anyway; the more common "madvise" policy leaves it to programs to
//! ask.)

use std::alloc::{GlobalAlloc, Layout, System};

pub struct Allocator;

// SAFETY: every request is passed on to the system allocator unchanged; the hint does not
// alter the content or the validity of a block.
unsafe impl GlobalAlloc for Allocator {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // SAFETY: the caller upholds the contract of `GlobalAlloc::alloc`.
        let block = unsafe { System.alloc(layout) };
        huge_pages::suggest(block, layout.size());

        block
    }

    #[inline]
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        // SAFETY: the caller upholds the contract of `GlobalAlloc::alloc_zeroed`.
        let block = unsafe { System.alloc_zeroed(layout) };
        huge_pages::suggest(block, layout.size());

        block
    }

    #[inline]
    unsafe fn dealloc(&self, block: *mut u8, layout: Layout) {
        // SAFETY: the caller upholds the contract of `GlobalAlloc::dealloc`.
        unsafe { System.dealloc(block, layout) }
    }

    #[inline]
    unsafe fn realloc(&self, block: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        // SAFETY: the caller upholds the contract of `GlobalAlloc::realloc`.
        let block = unsafe { System.realloc(block, layout, new_size) };
        huge_pages::suggest(block, new_size);

        block
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
