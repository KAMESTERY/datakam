//! Actix web Rusty WebApp
//!
//! A simple example integrating juniper in actix-web
//!

//#![warn(rust_2018_idioms)]

//extern crate wasm_bindgen;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate uuid;
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
extern crate data_encoding;
extern crate ring;
extern crate rusoto_core;
extern crate rusoto_dynamodb;
extern crate jsonwebtoken as jwt;
extern crate core;

mod schema;
mod dal;
mod security;
mod authentication;
mod validation;

//use wasm_bindgen::prelude::*;
use juniper::http::GraphQLRequest;

pub use schema::{create_schema, Schema};
pub use dal::{DynaDB};

//#[wasm_bindgen]
pub fn execute_query(query: String) -> String {
//pub unsafe extern "C" fn execute_query(query: String) -> String {

    info!("GQL Query: {}", query.clone());

    let gql_request: Option<GraphQLRequest> = serde_json::from_str(query.as_ref()).ok();
    match gql_request {
        Some(req) => {
            info!("GQL Request: {:?}", req.clone());
            let schema = create_schema();
            let result = req.execute(&schema, &());

            let json_string = serde_json::to_string(&result).ok();

            match json_string {
                Some(json) => {
                    info!("GQL Response: {}", json.clone());
                    json
                },
                None => "".to_string()
            }
        }
        None => {
            info!("GQL Response is nada!!");
            "".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static get_things_hhhh_query: &'static str = include_str!("get_things_hhhh_query.graphql");

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn test_query_execution() {

        let gql_response = execute_query(
            format!("{{\"query\": \"{}\"}}", get_things_hhhh_query.to_string())
        );
//        let gql_response = unsafe {
//            execute_query(
//                format!("{{\"query\": \"{}\"}}", get_things_hhhh_query.to_string())
//            )
//        };

        info!("GQL Response: {}", gql_response);

        assert!(!gql_response.is_empty());
    }
}
