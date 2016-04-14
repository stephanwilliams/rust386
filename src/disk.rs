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

enum DiskState {
    Idle,
    ReadSectors,
}

pub struct Disk {
    disk: Vec<u8>,
    command: [u8; 8],
    state: DiskState,

    count: u32,
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
            lba: 0,
        }
    }
}

impl IoDevice for Disk {
    const PORT_COUNT: u16 = 8;

    fn read_u8(&mut self, port: u16) -> u8 {
        trace!("read u8 {:04x}", port);
        match self.state {
            DiskState::Idle => {
                match port {
                    0...6 => self.command[port as usize],
                    7 => STATUS_DRDY.bits(),
                    _ => panic!("read invalid port")
                }
            },
            DiskState::ReadSectors => {
                0
            }
        }
    }

    fn write_u8(&mut self, port: u16, data: u8) {
        self.command[port as usize] = data;

        match self.state {
            DiskState::Idle => {
                if port == 7 {
                    match data {
                        0x20 => {
                            self.state = DiskState::ReadSectors;
                            self.count = self.command[2] as u32;
                            self.lba =
                                 (self.command[3] as u32) |
                                ((self.command[4] as u32) << 8) |
                                ((self.command[5] as u32) << 16) |
                                (((self.command[6] as u32) & 0b1111) << 24);
                        },
                        _ => {
                            panic!("invalid disk command")
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
