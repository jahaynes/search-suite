use bytes::*;

#[test]
pub fn roundtrip_u32() {
    let mut buf = Vec::<u8>::new();
    let mut p = 0;
    for x in 0..1024 {
        let sz_w      = write_esc_u32(&mut buf, x);
        let (y, sz_r) = read_esc_u32_at(&mut buf, p);
        assert!(x == y, "written value {} did not match read value {}", x, y);
        assert!(sz_w == sz_r, "written length {} did not match read length {}", sz_w, sz_r);
        p += sz_w;
    }
}

#[test]
pub fn roundtrip_u64() {
    let mut buf = Vec::<u8>::new();
    let mut p = 0;
    for x in 0..1024 {
        let sz_w      = write_esc_u64(&mut buf, x);
        let (y, sz_r) = read_esc_u64_at(&mut buf, p);
        assert!(x == y, "written value {} did not match read value {}", x, y);
        assert!(sz_w == sz_r, "written length {} did not match read length {}", sz_w, sz_r);
        p += sz_w;
    }
}