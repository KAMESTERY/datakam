
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate rocket;
extern crate rocket_contrib;

extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;

#[macro_use] extern crate log;

#[macro_use] extern crate error_chain;

#[macro_use] extern crate lazy_static;

extern crate dotenv;
extern crate ini;
extern crate chrono;
extern crate time;

extern crate curl;
extern crate url;
extern crate hyper;
extern crate openssl_probe;

extern crate r2d2_mysql;
extern crate r2d2_postgres;
extern crate r2d2_sqlite;
extern crate r2d2;

extern crate rusoto_core;
extern crate rusoto_s3;

extern crate regex;
extern crate indicatif;

use rocket::fairing::{AdHoc};

mod constants;
mod errors;
mod prelude;
mod macros;
mod utils;
mod api;
mod config;
mod dal;
mod operations;
mod endpoints;
use endpoints::{get_endpoints};

fn rocket() -> rocket::Rocket {
    config::prepare_environment();
    rocket::ignite().
        attach(AdHoc::on_launch(|_| {
            info!("Rocket Launching...");
        })).
        mount("/",
              get_endpoints())
}

fn main() {
    rocket().launch();
}
