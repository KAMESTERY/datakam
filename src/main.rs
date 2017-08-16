
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate rocket;
extern crate rocket_contrib;

#[macro_use] extern crate log;

mod endpoints;
use endpoints::{index, run_operations};

fn rocket() -> rocket::Rocket {
    rocket::ignite().
        mount("/",
              routes![index, run_operations])
}

fn main() {
    info!("Rocket Launching...");
    rocket().launch();
}
