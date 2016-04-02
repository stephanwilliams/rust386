use clock::{ Clocked };
use bus::{ BusState };

pub struct Intel80386 {

}

impl Intel80386 {
    pub fn new() -> Intel80386 {
        Intel80386 { }
    }
}

impl Clocked<BusState, BusState> for Intel80386 {
    fn rising_edge(&mut self, state: BusState) -> BusState {
        println!("cpu rising");
        state
    }

    fn falling_edge(&mut self, state: BusState) -> BusState {
        println!("cpu falling");
        state
    }
}
