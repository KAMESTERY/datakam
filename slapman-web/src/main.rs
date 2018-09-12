
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate openssl_probe;
extern crate actix;
extern crate actix_web;
extern crate handlebars;
#[macro_use]
extern crate serde_json;

extern crate worker_lib;

use actix::prelude::*;
use actix_web::{
    App, AsyncResponder, Error, FutureResponse, http, HttpRequest, HttpResponse, Json,
    middleware, server, State,
};
use handlebars::Handlebars;

use worker_lib::{DynaDB};
use std::env;

fn index(_req: &HttpRequest) -> Result<HttpResponse, Error> {
    Ok(HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body("Hello from SlapmanWeb!"))
}

fn dyna_tbls(_req: &HttpRequest) -> String {

    let table_names = DynaDB::list_tables();

    match table_names {
        Some(table_names) => format!("Table names: {:?}", table_names),
        None => String::from("No tables in database!")
    }
}

fn main() {
    openssl_probe::init_ssl_cert_env_vars();
    ::std::env::set_var("RUST_LOG", "actix_web,worker_lib,slapman_web=debug");
    env_logger::init();

    let sys = actix::System::new("SlapmanWeb_SystemRunner");

    let port = env::var("PORT")
        .unwrap_or_else(|_| "8090".to_string());
    let host = format!("localhost:{}", port.clone());

    // Start http server
    server::new(move || {
        App::new()
            // enable logger
            .middleware(middleware::Logger::default())
            .resource("/dynatables", |r| r.f(dyna_tbls))
            .resource("/", |r| r.f(index))
    }).bind(host.clone())
        .unwrap()
        .start();

    info!("{}", host);
    let _ = sys.run();
}
