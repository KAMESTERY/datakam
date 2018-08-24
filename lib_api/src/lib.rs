
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
use juniper::http::GraphQLRequest;

mod schema;
mod app_state;
mod dal;
mod security;
mod authentication;
mod validation;

use app_state::{AppState, GraphQLData, GraphQLExecutor};
use schema::create_schema;
use dal::{DynaDB};

pub fn execute_query(query: String) -> FutureResponse<HttpResponse> {
    ::std::env::set_var("RUST_LOG", "actix_web=debug");
    env_logger::init();
    let sys = actix::System::new("RustyWebApp_SystemRunner");

    let schema = std::sync::Arc::new(create_schema());
    let thread_count = num_cpus::get() * 2 + 1; // Choose just 3 by default ?
    let addr = SyncArbiter::start(
        thread_count,
        move || GraphQLExecutor::new(schema.clone())
    );

    println!("GQL Query: {}", query.clone());

    let gql_request: GraphQLData = serde_json::from_str(query.as_ref()).unwrap();

    println!("GQL Request: {:?}", gql_request.clone());

    let result = addr.send(gql_request)
        .from_err()
        .and_then(|res| match res {
            Ok(json_string) => Ok(HttpResponse::Ok()
                .content_type("application/json")
                .body(json_string)),
            Err(_) => Ok(HttpResponse::InternalServerError().into()),
        }).responder();

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn test_query_execution() {
        execute_query(
            String::from("{{ \"query\": \"\
                            query GetGThingsHHHH{{\
                              getLesChoses(\
                                userId: 'hhhh@hhhh.hhh',\
                                names: ['OneGThing', 'AnotherGThing']\
                              ) {{\
                                thing {{\
                                  name\
                                  userId\
                                  thingId\
                                  version\
                                  score\
                                  createdAt\
                                  updatedAt\
                                }}\
                                data {{\
                                  dataId\
                                  thingId\
                                  key\
                                  value\
                                }}\
                              }}\
                            }}\
            \"}}")
        );
        assert_eq!(2 + 2, 4);
    }
}
