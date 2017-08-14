
extern crate rayon;

use rayon::prelude::*;

#[no_mangle]
pub extern fn is_leap_year(year: i64) -> bool {
    let is_ly = year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
    return is_ly;
}

#[test]
fn verify_is_leap_year() {
    assert_eq!(true, is_leap_year(2012));
    assert_eq!(false, is_leap_year(2013));
    assert_eq!(false, is_leap_year(2014));
    assert_eq!(false, is_leap_year(2015));
    assert_eq!(true, is_leap_year(2016));
    assert_eq!(false, is_leap_year(2017));
    assert_eq!(false, is_leap_year(2018));
    assert_eq!(false, is_leap_year(2019));
}

#[no_mangle]
pub extern fn fibo(n : u64) -> u64 {

    if n < 2 {
        return 1;
    }
//    let mut prev1 = 1;
//    let mut prev2 = 1;
//    for _ in 1..n {
//        let new = prev1 + prev2;
//        prev2 = prev1;
//        prev1 = new;
//    }
//    prev1

    let (a, b) = rayon::join(
        || fibo(n - 2),
        || fibo(n - 1)
    );

    a + b
}

#[test]
fn verify_fibo() {
    assert_eq!(75025, fibo(24));
}
