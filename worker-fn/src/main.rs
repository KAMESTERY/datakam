//! Actix web Rusty WebApp
//!
//! A simple example integrating juniper in actix-web
//!

//#![warn(rust_2018_idioms)]

#[macro_use]
extern crate log;
extern crate actix;
extern crate actix_web;
extern crate env_logger;
extern crate core;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate juniper;
#[macro_use]
extern crate juniper_codegen;
extern crate num_cpus;
extern crate futures;
extern crate openssl_probe;
extern crate worker_lib;

mod app_state;

use actix::prelude::*;
use actix_web::{
    App, AsyncResponder, Error, FutureResponse, http, HttpRequest, HttpResponse, Json,
    middleware, server, State,
};
use futures::future::Future;
use juniper::http::graphiql::graphiql_source;

use app_state::{AppState, GraphQLData, GraphQLExecutor};
use worker_lib::create_schema;
use worker_lib::{DynaDB};
use std::env;

//fn index(_req: HttpRequest) -> &'static str {
fn index(_req: &HttpRequest<AppState>) -> Result<HttpResponse, Error> {
    Ok(HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body("Hello world!"))
}

fn counter(req: &HttpRequest<AppState>) -> Result<HttpResponse, Error> {
    let count = req.state().count.get() + 1; // <- get count
    req.state().count.set(count); // <- store new count in state

    Ok(HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body(format!("Request number: {}", count))) // <- response with count
}

fn dyna_tbls(_req: &HttpRequest<AppState>) -> String {

    let table_names = DynaDB::list_tables();

    match table_names {
        Some(table_names) => format!("Table names: {:?}", table_names),
        None => String::from("No tables in database!")
    }
}

fn read_tbl(_req: &HttpRequest<AppState>) -> Result<HttpResponse, Error> {

    Ok(HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body("fsgdfgf"))
}

fn graphiql(req: &HttpRequest<AppState>) -> Result<HttpResponse, Error> {
    let port: &String = &req.state().port;
    let html = graphiql_source(&format!("http://localhost:{}/graphql", port));
//    let html = graphiql_source("/graphql");
//    let html = graphiql_source("http://127.0.0.1:8088/graphql");
    Ok(HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body(html))
}

fn graphql(
    (st, data): (State<AppState>, Json<GraphQLData>),
) -> FutureResponse<HttpResponse> {
    st.executor
        .send(data.0)
        .from_err()
        .and_then(|res| match res {
            Ok(json_string) => Ok(HttpResponse::Ok()
                .content_type("application/json")
                .body(json_string)),
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        })
        .responder()
}

fn main() {
    openssl_probe::init_ssl_cert_env_vars();
    ::std::env::set_var("RUST_LOG", "actix_web,worker_lib,worker_fn=debug");
//    ::std::env::set_var("RUST_LOG", "rusoto,hyper,actix_web,worker_fn=debug");
    env_logger::init();
    let sys = actix::System::new("RustyWebApp_SystemRunner");

    let schema = std::sync::Arc::new(create_schema());
    let thread_count = num_cpus::get() * 2 + 1; // Choose just 3 by default ?
    let addr = SyncArbiter::start(
        thread_count,
        move || GraphQLExecutor::new(schema.clone())
    );

    let port = env::var("PORT")
        .unwrap_or_else(|_| "8088".to_string());
    let host = format!("localhost:{}", port.clone());

    // Start http server
    server::new(move || {
        App::with_state(AppState{
            executor: addr.clone(),
            count: std::cell::Cell::from(0),
            port: port.clone()
        })
            // enable logger
            .middleware(middleware::Logger::default())
            .resource("/graphql", |r| r.method(http::Method::POST).with(graphql))
            .resource("/explorer", |r| r.method(http::Method::GET).h(graphiql))
            .resource("/count", |r| r.method(http::Method::GET).f(counter))
            .resource("/dynatables", |r| r.f(dyna_tbls))
            .resource("/readtable", |r| r.f(read_tbl))
            .resource("/", |r| r.f(index))
    }).bind(host.clone())
        .unwrap()
        .start();

    info!("{}", host);
    let _ = sys.run();
}
