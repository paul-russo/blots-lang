//! The entry point of the process on Linux.
//!
//! The standard library has an entry point of its own, which runs before `main`, and the
//! first thing it wants to know is where the stack of the main thread ends, so that it can
//! tell a stack overflow from any other invalid access later on. glibc answers that by
//! reading and parsing `/proc/self/maps`, and for a short script that takes longer than
//! reading, compiling and running the script does. Being the `main` that the C runtime calls
//! leaves the standard entry point out, along with the question. What else it sets up is set
//! up here, to the same effect:
//!
//! - a standard stream that was closed is opened on `/dev/null`, so that no file the program
//!   opens later takes its place;
//! - `SIGPIPE` is ignored, so that writing to a closed pipe is an error rather than the end
//!   of the process;
//! - a stack overflow is reported as one before the process aborts;
//! - a panic is reported as one of the thread `main`, and ends the process with status 101.
//!
//! The arguments need no forwarding: glibc hands them to the initializers of a program, and
//! that is where the standard library takes them from.

use std::ffi::{c_char, c_int, c_long, c_void};
use std::io::{self, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::{env, mem, panic, process, ptr};

/// The size of the stack that the handler of an invalid access runs on, the one it may have
/// to report on being full.
///
/// The kernel saves the interrupted state there first, vector registers included, and what
/// that takes has grown with every extension of them: 12 KiB where it is largest today.
const HANDLER_STACK_SIZE: usize = 64 * 1024;

/// How far from the stack pointer an invalid access may be and still be taken for an overflow
/// of the stack.
///
/// A stack overflows at a push, or within the frame a function has just made for itself,
/// which it does a page at a time when the frame is larger than one. The kernel maps the
/// pages between the stack pointer and the limit of the stack as they are touched, so an
/// access this close that fails anyway has run into that limit.
const OVERFLOW_DISTANCE: usize = 64 * 1024;

const OVERFLOW_REPORT: &[u8] =
    b"\nthread 'main' has overflowed its stack\nfatal runtime error: stack overflow, aborting\n";

/// Zeroed, so it takes up no room in the binary, and is no memory at all until it is used.
static mut HANDLER_STACK: [u8; HANDLER_STACK_SIZE] = [0; HANDLER_STACK_SIZE];

#[unsafe(no_mangle)]
extern "C" fn main(_argc: c_int, _argv: *const *const c_char) -> c_int {
    // SAFETY: nothing else has run yet, so there is no other thread, no file of ours that a
    // descriptor could be confused with, and no handler that these would replace.
    unsafe {
        reopen_closed_streams();
        libc::signal(libc::SIGPIPE, libc::SIG_IGN);
        report_stack_overflows();
    }

    name_main_thread_in_panics();

    // The program leaves through `process::exit`, so it only comes back here by panicking,
    // and the panic has been reported by then.
    let _ = panic::catch_unwind(__rust_begin_short_backtrace);

    process::exit(101)
}

/// Run the program, as the function that a short backtrace ends at.
///
/// The standard hook leaves out the frames below the one of a function by this name, which is
/// how the frames of the standard entry point stay out of a backtrace, and those of this one.
#[inline(never)]
fn __rust_begin_short_backtrace() -> ! {
    crate::main()
}

/// Have a panic of the main thread reported under that name.
///
/// The standard hook knows the main thread by what the standard entry point remembers of it,
/// and calls it `<unnamed>` here. This one says what that one would have said where the
/// message is all there is to say: for the first panic, of the main thread, when no backtrace
/// is asked for. Anything else is left to the standard hook, which is the only one that can
/// print a backtrace the way it does.
fn name_main_thread_in_panics() {
    static PANICKED: AtomicBool = AtomicBool::new(false);

    let report = panic::take_hook();

    panic::set_hook(Box::new(move |info| {
        let plain = !PANICKED.swap(true, Ordering::Relaxed)
            && env::var_os("RUST_BACKTRACE").is_none_or(|style| style == "0")
            // SAFETY: both calls are always sound.
            && unsafe { libc::syscall(libc::SYS_gettid) == c_long::from(libc::getpid()) };

        match info.location() {
            Some(location) if plain => {
                let payload = info.payload();
                let message = payload
                    .downcast_ref::<&str>()
                    .copied()
                    .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
                    .unwrap_or("Box<dyn Any>");

                let report = format!(
                    "\nthread 'main' panicked at {location}:\n{message}\n\
                     note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace\n"
                );

                let _ = io::stderr().write_all(report.as_bytes());
            }
            _ => report(info),
        }
    }));
}

/// Open `/dev/null` in place of each of the standard streams that is closed.
///
/// SAFETY: to be called before anything is opened.
unsafe fn reopen_closed_streams() {
    let mut streams = [0, 1, 2].map(|fd| libc::pollfd {
        fd,
        events: 0,
        revents: 0,
    });

    // SAFETY: `streams` is three descriptors long.
    while unsafe { libc::poll(streams.as_mut_ptr(), 3, 0) } == -1 {
        // SAFETY: `__errno_location` returns the address of this thread's `errno`.
        match unsafe { *libc::__errno_location() } {
            libc::EINTR => continue,
            // The limit on open files can be too low for `poll` to look at three of them.
            libc::EINVAL | libc::EAGAIN | libc::ENOMEM => {
                for stream in &mut streams {
                    // SAFETY: asking for the flags of a descriptor is always sound.
                    let closed = unsafe { libc::fcntl(stream.fd, libc::F_GETFD) } == -1
                        && unsafe { *libc::__errno_location() } == libc::EBADF;

                    stream.revents = if closed { libc::POLLNVAL } else { 0 };
                }

                break;
            }
            // SAFETY: `abort` is always sound.
            _ => unsafe { libc::abort() },
        }
    }

    for stream in streams {
        // The lowest free descriptor is the one that is handed out, so opening anything fills
        // the gap. Leaving it open would let an unrelated file be read or written as a
        // standard stream.
        //
        // SAFETY: the path is a C string.
        if stream.revents & libc::POLLNVAL != 0
            && unsafe { libc::open(c"/dev/null".as_ptr(), libc::O_RDWR, 0) } == -1
        {
            // SAFETY: `abort` is always sound.
            unsafe { libc::abort() };
        }
    }
}

/// Have an overflow of the stack reported before it ends the process.
///
/// SAFETY: to be called once, by the main thread.
unsafe fn report_stack_overflows() {
    // There is no room left on a stack that has overflowed, so the handler needs another.
    let stack = libc::stack_t {
        ss_sp: (&raw mut HANDLER_STACK).cast(),
        ss_flags: 0,
        ss_size: HANDLER_STACK_SIZE,
    };

    // SAFETY: `HANDLER_STACK` is used for nothing else, and lives as long as the process.
    if unsafe { libc::sigaltstack(&stack, ptr::null_mut()) } != 0 {
        return;
    }

    // SAFETY: all zeroes is a `sigaction` with no flags and an empty mask, and `on_invalid_access`
    // has the signature that `SA_SIGINFO` asks for and only calls what a handler may call.
    unsafe {
        let mut action: libc::sigaction = mem::zeroed();
        action.sa_sigaction = on_invalid_access as *const () as libc::sighandler_t;
        action.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;

        let mut previous: libc::sigaction = mem::zeroed();
        libc::sigaction(libc::SIGSEGV, &action, &mut previous);

        // Whoever started the process may have asked for the signal to be ignored instead.
        if previous.sa_sigaction != libc::SIG_DFL {
            libc::sigaction(libc::SIGSEGV, &previous, ptr::null_mut());
        }
    }
}

/// Report an invalid access as a stack overflow if it is one, and let it end the process
/// either way.
unsafe extern "C" fn on_invalid_access(
    signal: c_int,
    info: *mut libc::siginfo_t,
    context: *mut c_void,
) {
    // SAFETY: the kernel passes what `SA_SIGINFO` promises: the details of the signal, and the
    // context that was interrupted.
    let (address, stack_pointer) =
        unsafe { ((*info).si_addr() as usize, stack_pointer(context.cast())) };

    if address
        .wrapping_sub(stack_pointer)
        .wrapping_add(OVERFLOW_DISTANCE)
        < 2 * OVERFLOW_DISTANCE
    {
        // SAFETY: the report is as long as is said, and both functions may be called from a
        // signal handler.
        unsafe {
            libc::write(
                libc::STDERR_FILENO,
                OVERFLOW_REPORT.as_ptr().cast(),
                OVERFLOW_REPORT.len(),
            );
            libc::abort();
        }
    }

    // Any other invalid access is not for this handler to explain. Returning runs the access
    // again, and with the default action back in place, it ends the process as it would have
    // without a handler.
    //
    // SAFETY: all zeroes is a `sigaction` with no flags and an empty mask.
    unsafe {
        let mut action: libc::sigaction = mem::zeroed();
        action.sa_sigaction = libc::SIG_DFL;
        libc::sigaction(signal, &action, ptr::null_mut());
    }
}

/// The stack pointer of an interrupted context.
///
/// SAFETY: `context` must be the context a signal handler was passed.
#[cfg(target_arch = "x86_64")]
unsafe fn stack_pointer(context: *const libc::ucontext_t) -> usize {
    // SAFETY: the caller passes a valid context.
    unsafe { (*context).uc_mcontext.gregs[libc::REG_RSP as usize] as usize }
}

/// The stack pointer of an interrupted context.
///
/// SAFETY: `context` must be the context a signal handler was passed.
#[cfg(target_arch = "aarch64")]
unsafe fn stack_pointer(context: *const libc::ucontext_t) -> usize {
    // SAFETY: the caller passes a valid context.
    unsafe { (*context).uc_mcontext.sp as usize }
}
