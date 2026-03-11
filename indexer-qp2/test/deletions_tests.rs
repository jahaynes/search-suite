#[cfg(test)]
use indexer_qp2::deletions::*;

#[cfg(test)]
#[test]
fn test_is_deleted2_bit_operations() {

    // Test byte 0, bit 0 (doc offset 0)
    let mut bytes = vec![0u8; 1];
    assert!(!is_deleted2(&bytes, 0));
    bytes[0] = 1; // Set bit 0
    assert!(is_deleted2(&bytes, 0));

    // Test byte 0, bit 1 (doc offset 1)
    let mut bytes = vec![0u8; 1];
    assert!(!is_deleted2(&bytes, 1));
    bytes[0] = 2; // Set bit 1
    assert!(is_deleted2(&bytes, 1));

    // Test byte 0, bit 7 (doc offset 7)
    let mut bytes = vec![0u8; 1];
    assert!(!is_deleted2(&bytes, 7));
    bytes[0] = 128; // Set bit 7
    assert!(is_deleted2(&bytes, 7));

    // Test byte 1, bit 0 (doc offset 8)
    let mut bytes = vec![0u8; 2];
    assert!(!is_deleted2(&bytes, 8));
    bytes[1] = 1; // Set bit 0 of byte 1
    assert!(is_deleted2(&bytes, 8));

    // Test multiple bits set
    let bytes = vec![255u8; 1]; // All bits set
    for i in 0..8 {
        assert!(is_deleted2(&bytes, i));
    }
}
