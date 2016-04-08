pub struct Rom {
    rom: Vec<u32>
}

impl Rom {
    pub fn new(data: &[u32], size: usize) -> Rom {
        let mut rom = vec![];
        rom.extend_from_slice(data);
        rom.resize(size, 0);
        Rom {
            rom: rom
        }
    }

    pub fn read(&self, paddr: u32) -> u32 {
        assert!(paddr & 0x3 == 0, "rom address must be 4 byte aligned");
        self.rom[(paddr >> 2) as usize]
    }

    pub fn size(&self) -> usize {
        self.rom.len()
    }
}
