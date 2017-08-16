
extern crate rayon;

//use rayon::prelude::*;

pub fn is_leap_year(year: i64) -> bool {
    let is_ly = year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
    return is_ly;
}

pub fn fibo(n : u64) -> u64 {

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

#[cfg(test)] mod tests;
