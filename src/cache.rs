const CACHE_SIZE: usize = 65536;
pub struct Cache {
    // 4 * 8096 = 32768 byte cache
    tags:  [Option<u32>; CACHE_SIZE],
    cache: [u32; CACHE_SIZE]
}

impl Cache {
    pub fn new() -> Cache {
        Cache {
            tags: [None; CACHE_SIZE],
            cache: [0; CACHE_SIZE]
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

        if let Some(t) = self.tags[ind] {
            if t != tag {
                debug!("cache evict {:08x} by {:08x}", t, tag);
            }
        }
        self.tags[ind] = Some(tag);
        self.cache[ind] = data;
    }

    pub fn flush(&mut self) {
        self.tags = [None; CACHE_SIZE];
        self.cache = [0; CACHE_SIZE];
    }
}
