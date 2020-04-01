extern crate env_logger;
extern crate sdl2;
mod chip;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use std::cmp;
use std::time::{Duration, Instant};

use chip::{Chip, SCREEN_HEIGHT, SCREEN_WIDTH};

const SCREEN_SCALING: usize = 10;
const BLACK: Color = Color::RGB(0, 0, 0);
const WHITE: Color = Color::RGB(255, 255, 255);

struct Timer {
    delay: Duration,
    next: Instant,
}

impl Timer {
    pub fn new(delay: Duration) -> Timer {
        Timer {
            delay,
            next: Instant::now() + delay,
        }
    }

    pub fn tick(&mut self) -> (bool, Duration) {
        let now = Instant::now();
        if now < self.next {
            (false, self.next - now)
        } else {
            self.next = now + self.delay;
            (true, self.delay)
        }
    }
}

struct Emu {
    canvas: sdl2::render::WindowCanvas,
    event_pump: sdl2::EventPump,
}

impl Emu {
    pub fn new() -> Emu {
        let sdl_context = sdl2::init().expect("cannot init sdl");
        let video_subsystem = sdl_context.video().expect("cannot init video");
        let window = video_subsystem
            .window(
                "chip8",
                (SCREEN_WIDTH * SCREEN_SCALING) as u32,
                (SCREEN_HEIGHT * SCREEN_SCALING) as u32,
            )
            .position_centered()
            .build()
            .unwrap();

        let canvas = window
            .into_canvas()
            .accelerated()
            .build()
            .expect("cannot init canvas");
        let event_pump = sdl_context.event_pump().expect("cannot init event_pump");

        Emu { canvas, event_pump }
    }

    fn run(&mut self, chip: &mut Chip) {
        self.refresh_screen(chip);
        let mut dec_timers = Timer::new(Duration::from_secs(1) / 60);
        let mut op_timer = Timer::new(Duration::from_secs(1) / 500);
        loop {
            for event in self.event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => return,
                    Event::KeyDown {
                        keycode: Some(kc),
                        repeat: false,
                        ..
                    } => match Emu::parse_keycode(kc) {
                        Some(v) => chip.press_key(v),
                        _ => (),
                    },
                    Event::KeyUp {
                        keycode: Some(kc),
                        repeat: false,
                        ..
                    } => match Emu::parse_keycode(kc) {
                        Some(v) => chip.release_key(v),
                        _ => (),
                    },
                    _ => (),
                }
            }

            // The rest of the game loop goes here...
            let (should_dec, next_dec) = dec_timers.tick();
            if should_dec {
                chip.dec_timers();
                self.refresh_screen(chip);
            }
            let (should_op, next_op) = op_timer.tick();
            if should_op {
                chip.next_op();
            }

            ::std::thread::sleep(cmp::min(next_dec, next_op));
        }
    }

    fn refresh_screen(&mut self, chip: &Chip) {
        self.canvas.set_draw_color(BLACK);
        self.canvas.clear();
        self.canvas.set_draw_color(WHITE);
        for p in chip.iter_pixels().filter(|p| p.v) {
            self.canvas
                .fill_rect(Rect::new(
                    (p.x * SCREEN_SCALING) as i32,
                    (p.y * SCREEN_SCALING) as i32,
                    SCREEN_SCALING as u32,
                    SCREEN_SCALING as u32,
                ))
                .expect("cannot draw rect");
        }
        self.canvas.present();
    }

    fn parse_keycode(kc: Keycode) -> Option<u8> {
        match kc {
            Keycode::X => Some(0x0),
            Keycode::Num1 => Some(0x1),
            Keycode::Num2 => Some(0x2),
            Keycode::Num3 => Some(0x3),
            Keycode::Q => Some(0x4),
            Keycode::W => Some(0x5),
            Keycode::E => Some(0x6),
            Keycode::A => Some(0x7),
            Keycode::S => Some(0x8),
            Keycode::D => Some(0x9),
            Keycode::Z => Some(0xA),
            Keycode::C => Some(0xB),
            Keycode::Num4 => Some(0xC),
            Keycode::R => Some(0xD),
            Keycode::F => Some(0xE),
            Keycode::V => Some(0xF),
            _ => None,
        }
    }
}

fn main() {
    env_logger::init();

    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        log::error!("missing file name");
        return;
    }
    let mut chip = Chip::new();
    chip.load_rom(&args[1]);

    let mut emu = Emu::new();
    emu.run(&mut chip);
}
