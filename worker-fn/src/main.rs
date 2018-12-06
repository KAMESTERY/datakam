//! Actix web Rusty WebApp
//!
//! A simple example integrating juniper in actix-web
//!

#[macro_use]
extern crate log;
#[macro_use]
extern crate serde_derive;

use std::env;

use actix;
use actix::prelude::*;
use actix_web::{
    App, AsyncResponder, Error, FutureResponse, http, HttpRequest, HttpResponse, Json,
    middleware, server, State,
};
use env_logger;
use futures::future::Future;
use juniper::http::graphiql::graphiql_source;
use num_cpus;
use openssl_probe;
use serde_json;

use worker_lib::create_schema;

use crate::app_state::{AppState, GraphQLData, GraphQLExecutor};

mod app_state;
mod api;

fn index(_req: &HttpRequest<AppState>) -> Result<HttpResponse, Error> {
    Ok(HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body("Hello from WorkerFn!"))
}

fn graphiql(_req: &HttpRequest<AppState>) -> Result<HttpResponse, Error> {
//    let port: &String = &req.state().port;
//    let html = graphiql_source(&format!("http://localhost:{}/graphql", port));
    let html = graphiql_source("/graphql");
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
    match ::std::env::var("RUST_LOG").ok() {
        Some(rust_log) => ::std::env::set_var("RUST_LOG", rust_log),
        None => ::std::env::set_var("RUST_LOG", "actix_web,worker_lib,worker_fn=debug")
    }
//    ::std::env::set_var("RUST_LOG", "rusoto,hyper,actix_web,worker_fn=debug");
    env_logger::init();
    let sys = actix::System::new("WorkerFn_SystemRunner");

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
            .resource("/", |r| r.f(index))
    }).bind(host.clone())
        .unwrap()
        .start();

    info!("{}", host);
    let _ = sys.run();
}
