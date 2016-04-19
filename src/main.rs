#![feature(trace_macros)]
#![feature(associated_consts)]

#![feature(alloc)]
#![feature(heap_api)]

#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(non_camel_case_types)]

extern crate alloc;

#[macro_use]
extern crate log;
extern crate env_logger;

#[macro_use]
extern crate enum_primitive;
extern crate num as numlib;

#[macro_use]
extern crate bitflags;

extern crate byteorder;

extern crate piston_window;

#[macro_use]
mod opcodes;

mod bus;
mod clock;
mod cpu;
mod cache;
mod ram;
mod reg;
mod rom;
mod instr;
mod iter;
mod iobus;
mod disk;
mod num;
mod cga;
mod display;

use bus::{ Bus, BusState };
use clock::{ HalfTimeClock, StandardClock, Clock, Clocked };
use cpu::{ Intel80386 };
use ram::{ MemoryController };
use rom::{ Rom };
use iter::{ DWords };
use iobus::{ IoBus };
use disk::{ Disk };
use cga::{ CGA, CGAController };
use display::{ Display };

use std::fs::{ File };
use std::io::{ Read };

fn main() {
    env_logger::init().unwrap();

    let mut cpu = Intel80386::new();

    let bios_bin = File::open("../bios/bios.bin").unwrap();
    let bios: Vec<u32> = DWords::new(bios_bin).collect();
    let bios_rom = Rom::new(&bios[..], 0x10000);
    cpu.map_rom(bios_rom, 0x000F0000..0x00100000);

    // ljmp 0x000F0000
    // ea 00 00 00 f0
    let reset_vector = vec![ 0x000000ea, 0x000000f0 ];
    let reset_vector_rom = Rom::new(&reset_vector[..], 0x10);
    cpu.map_rom(reset_vector_rom, 0xFFFFFFF0..0x100000000);

    let mut hclock: HalfTimeClock<BusState> = HalfTimeClock::new();

    hclock.add_clocked(cpu);

    let mut kernel_img = File::open("../kernel.img").unwrap();
    let mut kernel: Vec<u8> = vec![];
    kernel_img.read_to_end(&mut kernel).unwrap();
    let disk = Disk::new(kernel);

    let cgactl = CGAController::new();

    let mut iobus = IoBus::new();
    iobus.map_io(disk, 0x1F0..0x1F8);
    iobus.map_io(cgactl, 0x3D0..0x3E0);

    let memctl = MemoryController::new(4 * 1024 * 1024 * 1024);

    let mut bus = Bus::new();
    bus.add_clocked(hclock);
    bus.add_clocked(iobus);
    bus.add_clocked(memctl);

    let mut clock: StandardClock<()> = StandardClock::new();
    clock.add_clocked(bus);

    // let mut disp = Display::new();

    // let mut cga = CGA::new();

    // loop { if !disp.update(&cga).is_some() { return; } }

    loop {
        clock.rising_edge(());
        clock.falling_edge(());
    }
    
}
