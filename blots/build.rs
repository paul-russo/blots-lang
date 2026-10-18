//! Links the unwinder into the binary on Linux, where it is otherwise a shared library.
//!
//! A panic unwinds the stack with `libgcc_s.so`, which the dynamic loader has to find, map
//! and bind before the program starts, whether or not anything ever panics. That is a tenth
//! of the time it takes to run a short script. The same code comes as a static library with
//! every GCC, and linking that leaves nothing to load.
//!
//! It is only linked where it costs nothing else. Since GCC 12 the unwinder finds the unwind
//! tables of a function with `_dl_find_object`, which glibc has had since 2.35. A binary with
//! that unwinder inside does not start on an older glibc, where one that loads the shared
//! library gets the unwinder of the system it runs on, and starts on any glibc the rest of it
//! does.

use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=RUSTC_LINKER");

    let os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let libc = env::var("CARGO_CFG_TARGET_ENV").unwrap_or_default();

    if os == "linux"
        && libc == "gnu"
        && static_unwinder().is_some_and(|path| runs_on_any_glibc(&path))
    {
        println!("cargo:rustc-link-lib=static=gcc_eh");
    }
}

/// The compiler that links the binary, when it is known which one that is.
///
/// Cargo names the one that its configuration sets for the target. Without one, `rustc` links
/// with `cc`, which is the right compiler to ask only when it builds for the machine it
/// runs on.
fn linker() -> Option<OsString> {
    env::var_os("RUSTC_LINKER")
        .or_else(|| (env::var("HOST").ok()? == env::var("TARGET").ok()?).then(|| "cc".into()))
}

/// The `libgcc_eh.a` that the linker finds, if it finds one.
///
/// It is not there to be found when the toolchain is not GCC's, and the binary links against
/// the shared library then, as it would have without this script.
fn static_unwinder() -> Option<PathBuf> {
    let output = Command::new(linker()?)
        .arg("-print-file-name=libgcc_eh.a")
        .output()
        .ok()?;

    // A compiler answers with the path of a file it finds, and with the name it was asked
    // for otherwise.
    let path = PathBuf::from(String::from_utf8_lossy(&output.stdout).trim());

    (output.status.success() && path.is_absolute() && path.is_file()).then_some(path)
}

/// Whether the unwinder in the archive at `path` asks nothing of glibc that an old one lacks.
fn runs_on_any_glibc(path: &Path) -> bool {
    const NEWER: &[u8] = b"_dl_find_object";

    // The archive holds the names of the functions that its objects call as they are.
    fs::read(path).is_ok_and(|archive| !archive.windows(NEWER.len()).any(|name| name == NEWER))
}
