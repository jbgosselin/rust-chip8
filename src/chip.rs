extern crate log;
extern crate rand;

use rand::prelude::random;
use std::fmt;
use std::fs;

const PC_START: u16 = 0x200;
const CARRY: usize = 0xF;
const FONT: [[u8; 5]; 16] = [
    [0xF0, 0x90, 0x90, 0x90, 0xF0], // 0
    [0x20, 0x60, 0x20, 0x20, 0x70], // 1
    [0xF0, 0x10, 0xF0, 0x80, 0xF0], // 2
    [0xF0, 0x10, 0xF0, 0x10, 0xF0], // 3
    [0x90, 0x90, 0xF0, 0x10, 0x10], // 4
    [0xF0, 0x80, 0xF0, 0x10, 0xF0], // 5
    [0xF0, 0x80, 0xF0, 0x90, 0xF0], // 6
    [0xF0, 0x10, 0x20, 0x40, 0x40], // 7
    [0xF0, 0x90, 0xF0, 0x90, 0xF0], // 8
    [0xF0, 0x90, 0xF0, 0x10, 0xF0], // 9
    [0xF0, 0x90, 0xF0, 0x90, 0x90], // A
    [0xE0, 0x90, 0xE0, 0x90, 0xE0], // B
    [0xF0, 0x80, 0x80, 0x80, 0xF0], // C
    [0xE0, 0x90, 0x90, 0x90, 0xE0], // D
    [0xF0, 0x80, 0xF0, 0x80, 0xF0], // E
    [0xF0, 0x80, 0xF0, 0x80, 0x80], // F
];
pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_HEIGHT: usize = 32;
pub const STACK_SIZE: usize = 24;

struct Cmd(u8, u8, u8, u8);

impl Cmd {
    pub fn new(a: u8, b: u8) -> Cmd {
        Cmd(a >> 4 & 0xF, a & 0xF, b >> 4 & 0xF, b & 0xF)
    }

    #[inline(always)]
    fn x(&self) -> u8 {
        self.1
    }
    #[inline(always)]
    fn y(&self) -> u8 {
        self.2
    }
    #[inline(always)]
    fn n(&self) -> u8 {
        self.3
    }
    #[inline(always)]
    fn nn(&self) -> u8 {
        self.2 << 4 | self.3
    }
    #[inline(always)]
    fn nnn(&self) -> u16 {
        (self.1 as u16) << 8 | (self.2 as u16) << 4 | (self.3 as u16)
    }
}

impl fmt::Display for Cmd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Cmd(0x{:X?}{:X?}{:X?}{:X?})",
            self.0, self.1, self.2, self.3
        )
    }
}

struct Stack<T: Copy> {
    sp: usize,
    stack: Vec<T>,
}

impl<T: Copy + Default> Stack<T> {
    pub fn new(size: usize) -> Stack<T> {
        Stack {
            sp: 0,
            stack: vec![T::default(); size],
        }
    }

    pub fn push(&mut self, v: T) {
        if self.sp >= self.stack.len() {
            panic!("chip8 stack overflow");
        }
        self.stack[self.sp] = v;
        self.sp += 1;
    }

    pub fn pop(&mut self) -> T {
        if self.sp == 0 {
            panic!("chip8 pop empty stack");
        }
        self.sp -= 1;
        self.stack[self.sp]
    }
}

pub struct Pixel {
    pub x: usize,
    pub y: usize,
    pub v: bool,
}

struct Screen {
    width: usize,
    heigth: usize,
    buffer: Vec<Vec<u8>>,
}

impl Screen {
    pub fn new(width: usize, heigth: usize) -> Screen {
        assert_eq!(width % 8, 0);
        Screen {
            width,
            heigth,
            buffer: vec![vec![0x0; width / 8]; heigth],
        }
    }

    pub fn clear(&mut self) {
        self.buffer = vec![vec![0x0; self.width / 8]; self.heigth];
    }

    pub fn draw_sprite(&mut self, x: usize, y: usize, sprite: &[u8]) -> bool {
        let mut erased = false;
        let row_size = self.width / 8;
        let off: u8 = (x % 8) as u8;
        let x = (x % self.width) / 8;
        for (oy, row) in sprite.iter().enumerate() {
            let y = (y + oy) % self.heigth;
            // first
            let v = row >> off;
            erased |= self.buffer[y][x] & v != 0;
            self.buffer[y][x] ^= v;
            // second
            if off > 0 {
                let x = (x + 1) % row_size;
                let v = row << (8 - off);
                erased |= self.buffer[y][x] & v != 0;
                self.buffer[y][x] ^= v;
            }
        }
        erased
    }

    pub fn iter_pixels(&self) -> impl Iterator<Item = Pixel> + '_ {
        const MASK: u8 = 0x1 << 7;
        self.buffer.iter().enumerate().flat_map(|(y, row)| {
            row.iter().enumerate().flat_map(move |(x, chk)| {
                (0..8).map(move |off| Pixel {
                    y,
                    x: x * 8 + off,
                    v: chk & (MASK >> off) != 0,
                })
            })
        })
    }
}

pub struct Chip {
    mem: [u8; 4096],
    v: [u8; 16],
    i: u16,
    pc: u16,
    delay: u8,
    sound: u8,
    keys: u16,
    screen: Screen,
    stack: Stack<u16>,
    waiting_for_key: bool,
}

impl Chip {
    pub fn new() -> Chip {
        let mut chip = Chip {
            mem: [0; 4096],
            v: [0; 16],
            i: 0,
            pc: PC_START,
            delay: 0,
            sound: 0,
            keys: 0,
            screen: Screen::new(SCREEN_WIDTH, SCREEN_HEIGHT),
            stack: Stack::new(STACK_SIZE),
            waiting_for_key: false,
        };

        chip.init_font();
        chip
    }

    pub fn press_key(&mut self, key: u8) {
        self.keys |= 0x1u16 << key;
    }

    pub fn release_key(&mut self, key: u8) {
        self.keys &= !(0x1u16 << key);
    }

    fn get_key(&self, key: u8) -> bool {
        self.keys & (0x1u16 << key) != 0
    }

    pub fn iter_pixels(&self) -> impl Iterator<Item = Pixel> + '_ {
        self.screen.iter_pixels()
    }

    pub fn dec_timers(&mut self) {
        if self.waiting_for_key {
            return;
        }
        if self.delay > 0 {
            self.delay -= 1;
        }
        if self.sound > 0 {
            self.sound -= 1;
        }
    }

    pub fn next_op(&mut self) {
        if self.waiting_for_key {
            let pc = (self.pc - 2) as usize;
            let cmd = Cmd::new(self.mem[pc], self.mem[pc + 1]);
            self.wait_key_pressed(&cmd);
            return;
        }
        let pc = self.pc as usize;
        let cmd = Cmd::new(self.mem[pc], self.mem[pc + 1]);
        log::trace!("pc(0x{:04X}) {}", pc, cmd);
        log::trace!("i(0x{:04X}) d({}) s({})", self.i, self.delay, self.sound);
        log::trace!("reg({:?})", self.v);
        log::trace!("keys({:#016b})", self.keys);
        self.pc += 2;
        match cmd {
            Cmd(0x0, 0x0, 0xE, 0x0) => self.disp_clean(),
            Cmd(0x0, 0x0, 0xE, 0xE) => self.sub_return(),
            Cmd(0x1, _, _, _) => self.goto(&cmd),
            Cmd(0x2, _, _, _) => self.sub_call(&cmd),
            Cmd(0x3, _, _, _) => self.skip_eq_val(&cmd),
            Cmd(0x4, _, _, _) => self.skip_neq_val(&cmd),
            Cmd(0x5, _, _, 0x0) => self.skip_eq_reg(&cmd),
            Cmd(0x6, _, _, _) => self.store_val(&cmd),
            Cmd(0x7, _, _, _) => self.add_val(&cmd),
            Cmd(0x8, _, _, 0x0) => self.store_reg(&cmd),
            Cmd(0x8, _, _, 0x1) => self.or_reg(&cmd),
            Cmd(0x8, _, _, 0x2) => self.and_reg(&cmd),
            Cmd(0x8, _, _, 0x3) => self.xor_reg(&cmd),
            Cmd(0x8, _, _, 0x4) => self.add_reg(&cmd),
            Cmd(0x8, _, _, 0x5) => self.sub_reg(&cmd),
            Cmd(0x8, _, _, 0x6) => self.shift_right_reg(&cmd),
            Cmd(0x8, _, _, 0x7) => self.sub_reg_inv(&cmd),
            Cmd(0x8, _, _, 0xE) => self.sub_reg_inv(&cmd),
            Cmd(0x8, _, _, 0xF) => self.shift_left_reg(&cmd),
            Cmd(0x9, _, _, 0x0) => self.skip_neq_reg(&cmd),
            Cmd(0xA, _, _, _) => self.store_addr(&cmd),
            Cmd(0xB, _, _, _) => self.goto_add(&cmd),
            Cmd(0xC, _, _, _) => self.store_rand(&cmd),
            Cmd(0xD, _, _, _) => self.draw_sprite(&cmd),
            Cmd(0xE, _, 0x9, 0xE) => self.skip_key_pressed(&cmd),
            Cmd(0xE, _, 0xA, 0x1) => self.skip_key_not_pressed(&cmd),
            Cmd(0xF, _, 0x0, 0x7) => self.store_delay(&cmd),
            Cmd(0xF, _, 0x0, 0xA) => self.wait_key_pressed(&cmd),
            Cmd(0xF, _, 0x1, 0x5) => self.set_delay(&cmd),
            Cmd(0xF, _, 0x1, 0x8) => self.set_sound(&cmd),
            Cmd(0xF, _, 0x1, 0xE) => self.add_addr(&cmd),
            Cmd(0xF, _, 0x2, 0x9) => self.set_sprite_addr(&cmd),
            Cmd(0xF, _, 0x3, 0x3) => self.set_bcd(&cmd),
            Cmd(0xF, _, 0x5, 0x5) => self.reg_dump(&cmd),
            Cmd(0xF, _, 0x6, 0x5) => self.reg_load(&cmd),
            _ => log::warn!("unknown command {}", cmd),
        }
    }

    fn disp_clean(&mut self) {
        log::trace!("disp_clean");
        self.screen.clear();
    }

    fn sub_return(&mut self) {
        log::trace!("sub_return");
        self.pc = self.stack.pop();
    }

    fn goto(&mut self, cmd: &Cmd) {
        log::trace!("goto");
        self.pc = cmd.nnn();
    }

    fn sub_call(&mut self, cmd: &Cmd) {
        log::trace!("sub_call");
        self.stack.push(self.pc);
        self.pc = cmd.nnn();
    }

    fn skip_eq_val(&mut self, cmd: &Cmd) {
        log::trace!("skip_eq_val");
        let r = self.v[cmd.x() as usize];
        if r == cmd.nn() {
            self.pc += 2;
        }
    }

    fn skip_neq_val(&mut self, cmd: &Cmd) {
        log::trace!("skip_neq_val");
        let r = self.v[cmd.x() as usize];
        if r != cmd.nn() {
            self.pc += 2;
        }
    }

    fn skip_eq_reg(&mut self, cmd: &Cmd) {
        log::trace!("skip_eq_reg");
        let r1 = self.v[cmd.x() as usize];
        let r2 = self.v[cmd.y() as usize];
        if r1 == r2 {
            self.pc += 2;
        }
    }

    fn skip_neq_reg(&mut self, cmd: &Cmd) {
        log::trace!("skip_neq_reg");
        let r1 = self.v[cmd.x() as usize];
        let r2 = self.v[cmd.y() as usize];
        if r1 != r2 {
            self.pc += 2;
        }
    }

    fn store_val(&mut self, cmd: &Cmd) {
        log::trace!("store_val");
        self.v[cmd.x() as usize] = cmd.nn();
    }

    fn add_val(&mut self, cmd: &Cmd) {
        log::trace!("add_val");
        self.v[cmd.x() as usize] = self.v[cmd.x() as usize].wrapping_add(cmd.nn());
    }

    fn store_reg(&mut self, cmd: &Cmd) {
        log::trace!("store_reg");
        self.v[cmd.x() as usize] = self.v[cmd.y() as usize];
    }

    fn or_reg(&mut self, cmd: &Cmd) {
        log::trace!("or_reg");
        self.v[cmd.x() as usize] |= self.v[cmd.y() as usize];
    }

    fn and_reg(&mut self, cmd: &Cmd) {
        log::trace!("and_reg");
        self.v[cmd.x() as usize] &= self.v[cmd.y() as usize];
    }

    fn xor_reg(&mut self, cmd: &Cmd) {
        log::trace!("xor_reg");
        self.v[cmd.x() as usize] ^= self.v[cmd.y() as usize];
    }

    fn add_reg(&mut self, cmd: &Cmd) {
        log::trace!("add_reg");
        let (res, overflow) = self.v[cmd.x() as usize].overflowing_add(self.v[cmd.y() as usize]);
        self.v[cmd.x() as usize] = res;
        self.v[CARRY] = overflow as u8;
    }

    fn sub_reg(&mut self, cmd: &Cmd) {
        log::trace!("sub_reg");
        let (res, overflow) = self.v[cmd.x() as usize].overflowing_sub(self.v[cmd.y() as usize]);
        self.v[cmd.x() as usize] = res;
        self.v[CARRY] = !overflow as u8;
    }

    fn shift_right_reg(&mut self, cmd: &Cmd) {
        log::trace!("shift_right_reg");
        let r = self.v[cmd.x() as usize];
        self.v[CARRY] = r & 0x1;
        self.v[cmd.x() as usize] = r >> 1;
    }

    fn sub_reg_inv(&mut self, cmd: &Cmd) {
        log::trace!("sub_reg_inv");
        let (res, overflow) = self.v[cmd.y() as usize].overflowing_sub(self.v[cmd.x() as usize]);
        self.v[cmd.x() as usize] = res;
        self.v[CARRY] = !overflow as u8;
    }

    fn shift_left_reg(&mut self, cmd: &Cmd) {
        log::trace!("shift_left_reg");
        let r = self.v[cmd.x() as usize];
        self.v[CARRY] = r & 0x80;
        self.v[cmd.x() as usize] = r << 1;
    }

    fn store_addr(&mut self, cmd: &Cmd) {
        log::trace!("store_addr");
        self.i = cmd.nnn();
    }

    fn goto_add(&mut self, cmd: &Cmd) {
        log::trace!("goto_add");
        self.pc = (self.v[0] as u16) + cmd.nnn();
    }

    fn store_rand(&mut self, cmd: &Cmd) {
        log::trace!("store_rand");
        self.v[cmd.x() as usize] = random::<u8>() & cmd.nn();
    }

    fn draw_sprite(&mut self, cmd: &Cmd) {
        log::trace!("draw_sprite");
        let l = self.i as usize;
        let sprite = &self.mem[l..(l + (cmd.n() as usize))];
        let x = self.v[cmd.x() as usize];
        let y = self.v[cmd.y() as usize];
        let changed = self.screen.draw_sprite(x as usize, y as usize, sprite);
        self.v[CARRY] = changed as u8;
    }

    fn skip_key_pressed(&mut self, cmd: &Cmd) {
        log::trace!("skip_key_pressed");
        let r = self.v[cmd.x() as usize];
        if self.get_key(r) {
            self.pc += 2;
        }
    }

    fn skip_key_not_pressed(&mut self, cmd: &Cmd) {
        log::trace!("skip_key_not_pressed");
        let r = self.v[cmd.x() as usize];
        if !self.get_key(r) {
            self.pc += 2;
        }
    }

    fn store_delay(&mut self, cmd: &Cmd) {
        log::trace!("store_delay");
        self.v[cmd.x() as usize] = self.delay;
    }

    fn wait_key_pressed(&mut self, cmd: &Cmd) {
        log::trace!("wait_key_pressed");
        self.waiting_for_key = true;
        if let Some(k) = (0..16).filter(|k| self.get_key(*k)).nth(0) {
            self.v[cmd.x() as usize] = k;
            self.waiting_for_key = false;
        }
    }

    fn set_delay(&mut self, cmd: &Cmd) {
        log::trace!("set_delay");
        self.delay = self.v[cmd.x() as usize];
    }

    fn set_sound(&mut self, cmd: &Cmd) {
        log::trace!("set_sound");
        self.sound = self.v[cmd.x() as usize];
    }

    fn add_addr(&mut self, cmd: &Cmd) {
        log::trace!("add_addr");
        let (res, overflow) = self.i.overflowing_add(self.v[cmd.x() as usize] as u16);
        self.i = res;
        self.v[CARRY] = overflow as u8;
    }

    fn set_sprite_addr(&mut self, cmd: &Cmd) {
        log::trace!("set_sprite_addr");
        self.i = (self.v[cmd.x() as usize] as u16) * 5;
    }

    fn set_bcd(&mut self, cmd: &Cmd) {
        log::trace!("set_bcd");
        let l = self.i as usize;
        let val = self.v[cmd.x() as usize];
        self.mem[l] = val / 100;
        self.mem[l + 1] = (val / 10) % 10;
        self.mem[l + 2] = val % 10;
    }

    fn reg_dump(&mut self, cmd: &Cmd) {
        log::trace!("reg_dump");
        let l = self.i as usize;
        let x = (cmd.x() + 1) as usize;
        self.mem[l..(l + x)].copy_from_slice(&self.v[0..x]);
        self.i += x as u16;
    }

    fn reg_load(&mut self, cmd: &Cmd) {
        log::trace!("reg_load");
        let l = self.i as usize;
        let x = (cmd.x() + 1) as usize;
        self.v[0..x].copy_from_slice(&self.mem[l..(l + x)]);
        self.i += x as u16;
    }

    fn init_font(&mut self) {
        for (sprite, mem) in FONT.iter().zip(self.mem.chunks_mut(5)) {
            mem.copy_from_slice(sprite);
        }
    }

    pub fn load_rom(&mut self, path: &String) {
        let rom = fs::read(path).expect("unable to read rom");
        let start = PC_START as usize;
        self.mem[start..(start + rom.len())].copy_from_slice(rom.as_slice());
    }
}
