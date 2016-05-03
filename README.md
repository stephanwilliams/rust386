# Rust386

Rust 386 is a simple Intel 80386 emulator that supports only the minimum
features needed to boot the JOS operating system (as of Lab 1).

The code was written in the space of a month during school, so it contains a lot
of messy code and some rather questionable design decisions that probably
contribute to how slowly it runs (~70k IPS on my computer).

## Important Files

* `kernel.img`: The disk image used by the emulator, which contains the kernel.
* `bios/`: Contains the BIOS source and `bios.bin`, the BIOS image loaded into
ROM.

## Setup

Rust386 is written in Rust, and uses the Cargo package manager. Cargo is
included with Rust, so you can get both by installing the latest Rust from
https://www.rust-lang.org/downloads.html. Rust386 uses some unstable features so
you should download the Nightly build.

## Building and Running

Executing `cargo run --release` will fetch all dependencies, build the project,
and run it. This should be done from within the `src` folder (due to the
relative paths used for loading the bios, kernel, etc.).

Prepend `RUST_LOG=rust386=trace` to the `cargo` invocation to enable detailed
logging. Replace `trace` with `debug` for slightly less logging.
