#![feature(trace_macros)]

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
extern crate num;

#[macro_use]
extern crate bitflags;

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

use bus::{ Bus, BusState };
use clock::{ HalfTimeClock, StandardClock, Clock, Clocked };
use cpu::{ Intel80386 };
use ram::{ MemoryController };
use rom::{ Rom };

fn main() {
    env_logger::init().unwrap();

    let mut cpu = Intel80386::new();

    let bios = vec![0, 0, 0, 0];
    let bios_rom = Rom::new(&bios[..], 0x10000);
    cpu.map_rom(bios_rom, 0x000F0000..0x00100000);

    // ljmp 0x000F0000
    // ea 00 00 00 f0
    let reset_vector = vec![ 0x000000ea, 0x000000f0 ];
    let reset_vector_rom = Rom::new(&reset_vector[..], 0x10);
    cpu.map_rom(reset_vector_rom, 0xFFFFFFF0..0x100000000);

    let mut hclock: HalfTimeClock<BusState> = HalfTimeClock::new();

    hclock.add_clocked(cpu);

    let memctl = MemoryController::new(4 * 1024 * 1024 * 1024);

    let mut bus = Bus::new();
    bus.add_clocked(hclock);
    bus.add_clocked(memctl);

    let mut clock: StandardClock<()> = StandardClock::new();
    clock.add_clocked(bus);

    for _ in 0..8 {
        clock.rising_edge(());
        clock.falling_edge(());
    }
    
}
