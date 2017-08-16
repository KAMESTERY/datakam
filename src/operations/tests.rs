
use operations::{fibo, is_leap_year};

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

#[test]
fn verify_fibo() {
    assert_eq!(75025, fibo(24));
}
