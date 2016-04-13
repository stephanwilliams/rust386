use std::cell::{ RefCell };
use std::io::{ Read };
use byteorder::{ LittleEndian, ReadBytesExt };

pub struct DWords<R: Read> {
    reader: RefCell<Box<R>>
}

impl<R: Read> DWords<R> {
    pub fn new(reader: R) -> DWords<R> {
        DWords {
            reader: RefCell::new(Box::new(reader))
        }
    }

}

impl<R: Read> Iterator for DWords<R> {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        self.reader.borrow_mut().read_u32::<LittleEndian>().ok()
    }
}
