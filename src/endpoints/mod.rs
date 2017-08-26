
extern crate rayon;
//use rayon::prelude::*;

use rocket::{Route};
use rocket_contrib::Json;

use log::LogLevelFilter;

use config::Config;
use api::{Api, ApiResponse};

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

macro_rules! t {
    ($e:expr) => (match $e {
        Ok(e) => e,
        Err(e) => panic!("{} failed with {:?}", stringify!($e), e),
    })
}

#[get("/fetch/<baseurl>/<path>")]
fn fetch(baseurl: String, path: String) -> Option<Json<ApiResponse>> {

    let config = Config::new(baseurl);
    info!("Fetching with Config: {:?}", config);
    let api = Api::new(&config);
    match api.get(&path) {
        Ok(res) => Some(Json(res)),
        Err(e) => {
            error!("ERROR:::: {}", e);
            Option::None
        }
    }
}

pub fn get_endpoints() -> Vec<Route> {
    info!("Setting up Endpoints...");
    routes![index, run_operations, fetch]
}

#[cfg(test)] mod tests;
