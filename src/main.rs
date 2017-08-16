
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate rocket;
extern crate rocket_contrib;

#[macro_use] extern crate serde_derive;

#[macro_use] extern crate log;

extern crate time;

extern crate r2d2_mysql;
extern crate r2d2_postgres;
extern crate r2d2_sqlite;
extern crate r2d2;

mod dal;
mod operations;
mod endpoints;
use endpoints::{get_endpoints};

fn rocket() -> rocket::Rocket {
    info!("Rocket Launching...");
    rocket::ignite().
        mount("/",
              get_endpoints())
}

fn main() {
    rocket().launch();
}
