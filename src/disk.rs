use iobus::{ IoDevice };

bitflags! {
    pub flags Status: u8 {
        const STATUS_BSY  = 0b10000000,
        const STATUS_DRDY = 0b01000000,
        const STATUS_DF   = 0b00100000,
        const STATUS_SE   = 0b00100000,

        const STATUS_DRQ  = 0b00010000,
        const STATUS_DWE  = 0b00010000,
        const STATUS_SERV = 0b00010000,

        const STATUS_ERR  = 0b00000001,
    }
}

#[derive(Debug)]
enum DiskState {
    Idle,
    ReadSectors,
}

pub struct Disk {
    disk: Vec<u8>,
    command: [u8; 8],
    state: DiskState,

    count: u32,
    counter: u32,
    lba: u32
}

impl Disk {
    pub fn new(disk: Vec<u8>) -> Disk {
        assert!(disk.len() % 512 == 0);
        Disk {
            disk: disk,
            command: [0; 8],
            state: DiskState::Idle,

            count: 0,
            counter: 0,
            lba: 0,
        }
    }
}

impl IoDevice for Disk {
    const PORT_COUNT: u16 = 8;

    fn read_u8(&mut self, port: u16) -> u8 {
        trace!("read u8 {:02x} state {:?}", port, self.state);
        match self.state {
            DiskState::Idle => {
                match port {
                    0...6 => self.command[port as usize],
                    7 => STATUS_DRDY.bits(),
                    _ => panic!("read invalid port")
                }
            },
            DiskState::ReadSectors => {
                match port {
                    7 => STATUS_DRDY.bits(),
                    _ => panic!("read invalid port")
                }
            }
        }
    }

    fn read_u16(&mut self, port: u16) -> u16 {
        unimplemented!();
    }

    fn read_u32(&mut self, port: u16) -> u32 {
        match self.state {
            DiskState::ReadSectors => {
                match port {
                    0 => {
                        if self.counter < self.count {
                            let mut val = 0;
                            for i in 0..4 {
                                val |= 
                                    (self.disk[(self.lba * 512 + self.counter + i) as usize] as u32)
                                        << (8 * i);
                            }
                            self.counter += 4;
                            val
                        } else {
                            panic!("read too many bytes from disk");
                        }
                    },
                    _ => panic!("readsectors read u32 unexpected port")
                }
            },
            _ => panic!("read u32 unexpected state")
        }
    }

    fn write_u8(&mut self, port: u16, data: u8) {
        self.command[port as usize] = data;

        trace!("WRITE TO DISK");

        match self.state {
            DiskState::Idle => {
                if port == 7 {
                    match data {
                        0x20 => {
                            self.state = DiskState::ReadSectors;
                            let mut sectors = self.command[2] as u32;
                            if sectors == 0 {
                                sectors = 256;
                            }

                            self.count = sectors * 512;
                            self.counter = 0;

                            self.lba =
                                 (self.command[3] as u32) |
                                ((self.command[4] as u32) << 8) |
                                ((self.command[5] as u32) << 16) |
                                (((self.command[6] as u32) & 0b1111) << 24);
                        },
                        _ => {
                            panic!("invalid disk command {:x}", data)
                        }
                    }
                }
            },
            DiskState::ReadSectors => {
                panic!("write during readsectors")
            }
        }
    }
}
