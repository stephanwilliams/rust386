use std::cell::RefCell;

pub trait ClockState : Clone {
    fn coalesce(self, state: Self) -> Self;
    fn unit() -> Self;
}

impl ClockState for () {
    fn coalesce(self, state: ()) -> () {
        ()
    }

    fn unit() -> () {
        ()
    }
}

pub trait Clocked<S_in, S_out> where S_in: ClockState, S_out: ClockState {
    fn rising_edge(&mut self, state: S_in) -> S_out;
    fn falling_edge(&mut self, state: S_in) -> S_out;
}

pub trait Clock<S_in, S_out> where S_in: ClockState, S_out: ClockState {
    fn add_clocked<T>(&mut self, clocked: T) where T: Clocked<S_in, S_out>;
}

pub struct StandardClock<'a, S> where S: ClockState {
    clockeds: Vec<RefCell<Box<Clocked<S, S> + 'a>>>
}

impl<'a, S> StandardClock<'a, S> where S: ClockState {
    pub fn new() -> StandardClock<'a, S> {
        StandardClock {
            clockeds: vec![]
        }
    }
}

impl<'a, S> Clock<S, S> for StandardClock<'a, S> where S: ClockState {
    fn add_clocked<T>(&mut self, clocked: T) where T: Clocked<S, S> + 'a {
        self.clockeds.push(RefCell::new(Box::new(clocked)));
    }
}

impl<'a, S> Clocked<S, S> for StandardClock<'a, S> where S: ClockState {
    fn rising_edge(&mut self, state: S) -> S {
        let mut s = S::unit();

        for clocked in self.clockeds.iter_mut() {
            let cmut = &mut *clocked.borrow_mut();
            s = s.coalesce(cmut.rising_edge(state.clone()));
        }

        s
    }

    fn falling_edge(&mut self, state: S) -> S {
        let mut s = S::unit();

        for clocked in self.clockeds.iter_mut() {
            let cmut = &mut *clocked.borrow_mut();
            s = s.coalesce(cmut.falling_edge(state.clone()));
        }

        s
    }
}

pub struct HalfTimeClock<'a, S> where S: ClockState {
    clockeds: Vec<RefCell<Box<Clocked<S, S> + 'a>>>,
    rising: bool,
    last_state: S
}

impl<'a, S> HalfTimeClock<'a, S> where S: ClockState {
    pub fn new() -> HalfTimeClock<'a, S> {
        HalfTimeClock {
            clockeds: vec![],
            rising: false,
            last_state: S::unit()
        }
    }
}

impl<'a, S> Clock<S, S> for HalfTimeClock<'a, S> where S: ClockState {
    fn add_clocked<T>(&mut self, clocked: T) where T: Clocked<S, S> + 'a {
        self.clockeds.push(RefCell::new(Box::new(clocked)));
    }
}

impl<'a, S> Clocked<S, S> for HalfTimeClock<'a, S> where S: ClockState {
    fn rising_edge(&mut self, state: S) -> S {
        self.rising = !self.rising;

        let mut s = S::unit();

        for clocked in self.clockeds.iter_mut() {
            let cmut = &mut *clocked.borrow_mut();
            if self.rising {
                s = s.coalesce(cmut.rising_edge(state.clone()));
            } else {
                s = s.coalesce(cmut.falling_edge(state.clone()));
            }
        }

        self.last_state = s.clone();

        s
    }
    
    fn falling_edge(&mut self, state: S) -> S {
        self.last_state.clone()
    }
}
