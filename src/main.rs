mod bus;
mod clock;
mod cpu;

use bus::{ Bus, BusState };
use clock::{ HalfTimeClock, StandardClock, Clock, Clocked };
use cpu::{ Intel80386 };

fn main() {
    println!("Hello, world!");

    let mut cpu = Intel80386::new();
    let mut hclock: HalfTimeClock<BusState> = HalfTimeClock::new();

    hclock.add_clocked(cpu);

    let mut bus = Bus::new();
    bus.add_clocked(hclock);

    let mut clock: StandardClock<()> = StandardClock::new();
    clock.add_clocked(bus);

    for i in 0..8 {
        clock.rising_edge(());
        clock.falling_edge(());
    }
    
}
