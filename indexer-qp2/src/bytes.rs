use byteorder::{ByteOrder, LittleEndian};

// Encode unescaped with correct endian-ness
pub fn write_esc_u32(buf: &mut Vec<u8>,
                     x:   u32) -> usize {

    let mut unescaped_buf = [0; 4];
    LittleEndian::write_u32(&mut unescaped_buf, x);
    write_esc(buf, &unescaped_buf)
}

// Encode unescaped with correct endian-ness
pub fn write_esc_u64(buf: &mut Vec<u8>,
                     x:   u64) -> usize {
    let mut unescaped_buf = [0; 8];
    LittleEndian::write_u64(&mut unescaped_buf, x);
    write_esc(buf, &unescaped_buf)
}

pub fn write_esc(buf:   &mut Vec<u8>,
                 bytes: &[u8]) -> usize {
    let mut written = 0;
    for b in bytes {
        match *b {
            10 => { buf.push(b'\\');
                    buf.push(b'n');
                    written += 2;
                  },
            13 => { buf.push(b'\\');
                    buf.push(b'r');
                    written += 2;
                  },
            92 => { buf.push(b'\\');
                    buf.push(b'\\');
                    written += 2;
                  },
            _  => { buf.push(*b);
                    written += 1; 
                  }
        }
    }
    written
}

pub fn read_esc_u32_at(input: &[u8], j: usize) -> (u32, usize) {
    let mut buf  = Vec::<u8>::new();
    let mut esc  = false;
    let mut read = 0;
    let mut i    = j;
    while read < 4 {
        let c = input[i];
        i += 1;
        if esc {
            esc = false;
            match c {
                b'n'  => buf.push(10),
                b'r'  => buf.push(13),
                b'\\' => buf.push(92),
                o     => panic!("Invalid escape!: {}", o)
            }
            read += 1;
        } else {
            if c == 92 {
                esc = true;
            } else {
                buf.push(c);
                read += 1;
            }
        }
    }
    (LittleEndian::read_u32(&buf), i - j)
}

pub fn read_esc_u64_at(input: &[u8], j: usize) -> (u64, usize) {
    let mut buf  = Vec::<u8>::new();
    let mut esc  = false;
    let mut read = 0;
    let mut i    = j;
    while read < 8 {
        let c = input[i];
        i += 1;
        if esc {
            esc = false;
            match c {
                b'n'  => buf.push(10),
                b'r'  => buf.push(13),
                b'\\' => buf.push(92),
                o     => panic!("Invalid escape!: {}", o)
            }
            read += 1;
        } else {
            if c == 92 {
                esc = true;
            } else {
                buf.push(c);
                read += 1;
            }
        }
    }
    (LittleEndian::read_u64(&buf), i - j)
}