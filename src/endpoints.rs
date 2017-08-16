
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate rocket;
extern crate rocket_contrib;

#[macro_use] extern crate log;

#[macro_use] extern crate log;

#[macro_use] extern crate serde_derive;

extern crate rayon;

extern crate serde_json;

use rocket_contrib::Json;
//use rayon::prelude::*;

mod operations;
use operations::{fibo, is_leap_year};

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[derive(Serialize)]
struct Ops {
    fib_n: u64,
    fib_of_n: u64,
    year: i64,
    is_leap: bool
}

#[get("/operations/<fib_n>/<year>")]
fn run_operations(fib_n: u64, year: i64) -> Json<Ops> {

    let (fib_of_n, is_leap) = rayon::join(
        || fibo(fib_n),
        || is_leap_year(year),
    );

    info!(
        "Fibonacci of {} is {} and the year {} is leap year = {}",
        fib_n, fib_of_n, year, is_leap
    );

    Json(
        Ops {
            fib_n: fib_n,
            fib_of_n: fib_of_n,
            year: year,
            is_leap: is_leap
        }
    )
}