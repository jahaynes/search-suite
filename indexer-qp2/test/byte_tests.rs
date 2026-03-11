#[cfg(test)]
use indexer_qp2::bytes::*;

#[cfg(test)]
#[test]
fn roundtrip_u32() {
    let mut buf = Vec::<u8>::new();
    let mut p = 0;
    for x in 0..1024 {
        let sz_w = write_esc_u32(&mut buf, x);
        let (y, sz_r) = read_esc_u32_at(&mut buf, p);
        assert!(x == y, "written value {} did not match read value {}", x, y);
        assert!(
            sz_w == sz_r,
            "written length {} did not match read length {}",
            sz_w,
            sz_r
        );
        p += sz_w;
    }
}

#[cfg(test)]
#[test]
pub fn roundtrip_u64() {
    let mut buf = Vec::<u8>::new();
    let mut p = 0;
    for x in 0..1024 {
        let sz_w = write_esc_u64(&mut buf, x);
        let (y, sz_r) = read_esc_u64_at(&mut buf, p);
        assert!(x == y, "written value {} did not match read value {}", x, y);
        assert!(
            sz_w == sz_r,
            "written length {} did not match read length {}",
            sz_w,
            sz_r
        );
        p += sz_w;
    }
}

#[cfg(test)]
#[test]
fn test_write_esc_u32_edge_cases() {
    // Test minimum value
    let mut buf = Vec::new();
    let sz = write_esc_u32(&mut buf, 0);
    assert_eq!(sz, 4);
    assert_eq!(buf, vec![0, 0, 0, 0]);

    // Test maximum 32-bit value
    let mut buf = Vec::new();
    let sz = write_esc_u32(&mut buf, u32::MAX);
    assert_eq!(sz, 4);
    assert_eq!(buf, vec![255, 255, 255, 255]);

    // Test common values
    let mut buf = Vec::new();
    let sz = write_esc_u32(&mut buf, 1);
    assert_eq!(sz, 4);
    assert_eq!(buf, vec![1, 0, 0, 0]);

    let mut buf = Vec::new();
    let sz = write_esc_u32(&mut buf, 256);
    assert_eq!(sz, 4);
    assert_eq!(buf, vec![0, 1, 0, 0]);

    let mut buf = Vec::new();
    let sz = write_esc_u32(&mut buf, 65536);
    assert_eq!(sz, 4);
    assert_eq!(buf, vec![0, 0, 1, 0]);
}

#[cfg(test)]
#[test]
fn test_read_esc_u32_at_edge_cases() {
    // Test reading 0
    let buf = vec![0, 0, 0, 0];
    let (val, read) = read_esc_u32_at(&buf, 0);
    assert_eq!(val, 0);
    assert_eq!(read, 4);

    // Test reading u32::MAX
    let buf = vec![255, 255, 255, 255];
    let (val, read) = read_esc_u32_at(&buf, 0);
    assert_eq!(val, u32::MAX);
    assert_eq!(read, 4);

    // Test reading 1
    let buf = vec![1, 0, 0, 0];
    let (val, read) = read_esc_u32_at(&buf, 0);
    assert_eq!(val, 1);
    assert_eq!(read, 4);

    // Test reading 256
    let buf = vec![0, 1, 0, 0];
    let (val, read) = read_esc_u32_at(&buf, 0);
    assert_eq!(val, 256);
    assert_eq!(read, 4);
}

#[cfg(test)]
#[test]
fn test_roundtrip_u32_edge_cases() {
    let test_values = vec![
        0,
        1,
        255,
        256,
        65535,
        65536,
        16777215,
        16777216,
        u32::MAX - 1,
        u32::MAX,
    ];

    for val in test_values {
        let mut buf = Vec::new();
        let sz_w = write_esc_u32(&mut buf, val);
        let (read_val, sz_r) = read_esc_u32_at(&mut buf, 0);
        assert_eq!(val, read_val);
        assert_eq!(sz_w, sz_r);
    }
}

#[cfg(test)]
#[test]
fn test_write_esc_u64_edge_cases() {
    // Test minimum value
    let mut buf = Vec::new();
    let sz = write_esc_u64(&mut buf, 0);
    assert_eq!(sz, 8);
    assert_eq!(buf, vec![0, 0, 0, 0, 0, 0, 0, 0]);

    // Test maximum 64-bit value
    let mut buf = Vec::new();
    let sz = write_esc_u64(&mut buf, u64::MAX);
    assert_eq!(sz, 8);
    assert_eq!(buf, vec![255, 255, 255, 255, 255, 255, 255, 255]);

    // Test common values
    let mut buf = Vec::new();
    let sz = write_esc_u64(&mut buf, 1);
    assert_eq!(sz, 8);
    assert_eq!(buf, vec![1, 0, 0, 0, 0, 0, 0, 0]);

    let mut buf = Vec::new();
    let sz = write_esc_u64(&mut buf, 65536);
    assert_eq!(sz, 8);
    assert_eq!(buf, vec![0, 0, 1, 0, 0, 0, 0, 0]);
}

#[cfg(test)]
#[test]
fn test_read_esc_u64_at_edge_cases() {
    // Test reading 0
    let buf = vec![0, 0, 0, 0, 0, 0, 0, 0];
    let (val, read) = read_esc_u64_at(&buf, 0);
    assert_eq!(val, 0);
    assert_eq!(read, 8);

    // Test reading u64::MAX
    let buf = vec![255, 255, 255, 255, 255, 255, 255, 255];
    let (val, read) = read_esc_u64_at(&buf, 0);
    assert_eq!(val, u64::MAX);
    assert_eq!(read, 8);

    // Test reading 1
    let buf = vec![1, 0, 0, 0, 0, 0, 0, 0];
    let (val, read) = read_esc_u64_at(&buf, 0);
    assert_eq!(val, 1);
    assert_eq!(read, 8);

    // Test reading 65536
    let buf = vec![0, 0, 1, 0, 0, 0, 0, 0];
    let (val, read) = read_esc_u64_at(&buf, 0);
    assert_eq!(val, 65536);
    assert_eq!(read, 8);
}

#[cfg(test)]
#[test]
fn test_roundtrip_u64_edge_cases() {
    let test_values = vec![
        0,
        1,
        65535,
        65536,
        4294967295,
        4294967296,
        u64::MAX - 1,
        u64::MAX,
    ];

    for val in test_values {
        let mut buf = Vec::new();
        let sz_w = write_esc_u64(&mut buf, val);
        let (read_val, sz_r) = read_esc_u64_at(&mut buf, 0);
        assert_eq!(val, read_val);
        assert_eq!(sz_w, sz_r);
    }
}

#[cfg(test)]
#[test]
fn test_write_esc_special_bytes() {
    // Test newline (10) - should be escaped as \n
    let mut buf = Vec::new();
    let sz = write_esc(&mut buf, &[10]);
    assert_eq!(sz, 2);
    assert_eq!(buf, vec![b'\\', b'n']);

    // Test carriage return (13) - should be escaped as \r
    let mut buf = Vec::new();
    let sz = write_esc(&mut buf, &[13]);
    assert_eq!(sz, 2);
    assert_eq!(buf, vec![b'\\', b'r']);

    // Test backslash (92) - should be escaped as \\
    let mut buf = Vec::new();
    let sz = write_esc(&mut buf, &[92]);
    assert_eq!(sz, 2);
    assert_eq!(buf, vec![b'\\', b'\\']);
}

#[cfg(test)]
#[test]
fn test_write_esc_normal_bytes() {
    // Test normal bytes - should pass through unchanged
    let mut buf = Vec::new();
    let sz = write_esc(&mut buf, &[65, 66, 67]); // "ABC"
    assert_eq!(sz, 3);
    assert_eq!(buf, vec![65, 66, 67]);

    // Test mixed normal and special
    let mut buf = Vec::new();
    let sz = write_esc(&mut buf, &[65, 10, 66]); // "A\nB"
    assert_eq!(sz, 4);
    assert_eq!(buf, vec![65, b'\\', b'n', 66]);
}

#[cfg(test)]
#[test]
fn test_read_esc_u32_at_invalid_escape() {
    // Test invalid escape character
    let buf = vec![b'\\', b'x', 0, 0, 0]; // \x is invalid

    // Should panic on invalid escape
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        read_esc_u32_at(&buf, 0);
    }))
    .expect_err("Should have panicked on invalid escape");
}

#[cfg(test)]
#[test]
fn test_write_esc_multiple_special_chars() {
    // Test multiple special characters in sequence
    let mut buf = Vec::new();
    let sz = write_esc(&mut buf, &[10, 13, 92, 10, 13, 92]);
    assert_eq!(sz, 12); // Each special char becomes 2 bytes
    assert_eq!(
        buf,
        vec![
            b'\\', b'n', b'\\', b'r', b'\\', b'\\', b'\\', b'n', b'\\', b'r', b'\\', b'\\'
        ]
    );
}

#[cfg(test)]
#[test]
fn test_roundtrip_with_special_chars() {
    // Test roundtrip with various byte values including special ones
    let test_bytes = vec![
        vec![0, 1, 2, 3],
        vec![9, 10, 11, 12],  // tab, newline, etc.
        vec![13, 91, 92, 93], // cr, [, \\, ]
        vec![254, 255],
    ];

    for bytes in test_bytes {
        let mut buf = Vec::new();
        let _sz_w = write_esc(&mut buf, &bytes);
        let mut read_buf = Vec::new();
        let mut esc = false;
        let mut read = 0;
        let mut i = 0;

        while read < bytes.len() {
            let c = buf[i];
            i += 1;
            if esc {
                esc = false;
                match c {
                    b'n' => read_buf.push(10),
                    b'r' => read_buf.push(13),
                    b'\\' => read_buf.push(92),
                    _ => panic!("Invalid escape"),
                }
                read += 1;
            } else {
                if c == 92 {
                    esc = true;
                } else {
                    read_buf.push(c);
                    read += 1;
                }
            }
        }

        assert_eq!(bytes, read_buf);
    }
}
