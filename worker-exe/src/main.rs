//! Actix web Rusty WebApp
//!
//! A simple example integrating juniper in actix-web
//!

//#![warn(rust_2018_idioms)]

extern crate actix;
extern crate actix_web;
extern crate env_logger;
extern crate uuid;
extern crate futures;
extern crate rayon;
#[macro_use]
extern crate validator_derive;
extern crate validator;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
#[macro_use]
extern crate juniper;
#[macro_use]
extern crate juniper_codegen;
extern crate num_cpus;
extern crate data_encoding;
extern crate ring;
extern crate rusoto_core;
extern crate rusoto_dynamodb;
extern crate jsonwebtoken as jwt;
extern crate core;

use actix::prelude::*;
use actix_web::{
    App, AsyncResponder, Error, FutureResponse, http, HttpRequest, HttpResponse, Json,
    middleware, server, State,
};
use futures::future::Future;
use juniper::http::graphiql::graphiql_source;

mod schema;
mod app_state;
mod dal;
mod security;
mod authentication;
mod validation;

use app_state::{AppState, GraphQLData, GraphQLExecutor};
use schema::create_schema;
use dal::{DynaDB};

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

fn graphiql(_req: &HttpRequest<AppState>) -> Result<HttpResponse, Error> {
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
    ::std::env::set_var("RUST_LOG", "actix_web=debug");
    env_logger::init();
    let sys = actix::System::new("RustyWebApp_SystemRunner");

    let schema = std::sync::Arc::new(create_schema());
    let thread_count = num_cpus::get() * 2 + 1; // Choose just 3 by default ?
    let addr = SyncArbiter::start(
        thread_count,
        move || GraphQLExecutor::new(schema.clone())
    );

    // Start http server
    server::new(move || {
        App::with_state(AppState{executor: addr.clone(), count: std::cell::Cell::from(0)})
            // enable logger
            .middleware(middleware::Logger::default())
            .resource("/graphql", |r| r.method(http::Method::POST).with(graphql))
            .resource("/graphiql", |r| r.method(http::Method::GET).h(graphiql))
            .resource("/count", |r| r.method(http::Method::GET).f(counter))
            .resource("/dynatables", |r| r.f(dyna_tbls))
            .resource("/readtable", |r| r.f(read_tbl))
            .resource("/", |r| r.f(index))
    }).bind("127.0.0.1:8088")
        .unwrap()
        .start();

    println!("Started http server: 127.0.0.1:8088");
    let _ = sys.run();
}

// Shelving this for now
//#[no_mangle]
//pub unsafe extern "C" fn execute_query(query: String) -> String {
//
//    debug!("GQL Query: {}", query.clone());
//
//    let gql_request: Option<GraphQLRequest> = serde_json::from_str(query.as_ref()).ok();
//    match gql_request {
//        Some(req) => {
//            debug!("GQL Request: {:?}", req.clone());
//            let schema = create_schema();
//            let result = req.execute(&schema, &());
//
//            let json_string = serde_json::to_string(&result).ok();
//
//            match json_string {
//                Some(json) => json,
//                None => "".to_string()
//            }
//        }
//        None => "".to_string()
//    }
//}
