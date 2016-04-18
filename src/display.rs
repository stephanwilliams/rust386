use std::path::Path;
use std::str;

use piston_window::*;
use cga::{ CGA, CRT_ROWS, CRT_COLS };

pub struct Display {
    window: PistonWindow
}


impl Display {
    pub fn new() -> Display {
        Display {
            window: WindowSettings::new("rust386", (1024, 768))
                .exit_on_esc(true)
                .build()
                .unwrap_or_else(|e| { panic!("Failed to build PistonWindow: {}", e) })
        }
    }

    pub fn update(&mut self, cga: &CGA) -> Option<()> {
        if let Some(window) = self.window.next() {
            self.window = window;

            let t = Text::new_color(color::WHITE, 16);
            let mut cache = Glyphs::new(
                &Path::new("/Library/Fonts/Courier New.ttf"),
                self.window.factory.borrow_mut().clone()
                ).ok().unwrap();

            self.window.draw_2d(|_c, g| {
                clear([0.2, 0.2, 0.2, 1.0], g);
                for row in 0..CRT_ROWS {
                    for col in 0..CRT_COLS {
                        let off = 2 * (row * CRT_COLS + col);
                        let ch = cga.read_u8(off);
                        if ch == 0 { continue; }
                        t.draw(
                            str::from_utf8(&[ch]).unwrap(),
                            &mut cache,
                            &_c.draw_state,
                            _c.transform.trans(
                                32.0 + 12.0 * col as f64,
                                30.0 * (row + 1) as f64),
                            g);
                    }
                }
            });

            return Some(());
        }

        None
    }
}
