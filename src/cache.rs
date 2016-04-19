pub struct Cache {
    // 4 * 8096 = 32768 byte cache
    tags:  [Option<u32>; 8096],
    cache: [u32; 8096]
}

impl Cache {
    pub fn new() -> Cache {
        Cache {
            tags: [None; 8096],
            cache: [0; 8096]
        }
    }

    pub fn read(&self, paddr: u32) -> Option<u32> {
        assert!(paddr & 0x3 == 0, "non-aligned cache read");

        let tag = paddr >> 2;
        let ind = (tag % self.tags.len() as u32) as usize;

        if let Some(cur_tag) = self.tags[ind] {
            if cur_tag == tag {
                return Some(self.cache[ind]);
            }
        }

        None
    }

    pub fn write(&mut self, paddr: u32, data: u32) {
        assert!(paddr & 0x3 == 0, "non-aligned cache write");

        let tag = paddr >> 2;
        let ind = (tag % self.tags.len() as u32) as usize;

        self.tags[ind] = Some(tag);
        self.cache[ind] = data;
    }

    pub fn flush(&mut self) {
        self.tags = [None; 8096];
        self.cache = [0; 8096];
    }
}
