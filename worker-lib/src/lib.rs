//! Actix web Rusty WebApp
//!
//! A simple example integrating juniper in actix-web
//!

#[macro_use]
extern crate juniper;
#[macro_use]
extern crate juniper_codegen;
//extern crate wasm_bindgen;
#[macro_use]
extern crate log;
#[macro_use]
extern crate serde_derive;


use jsonwebtoken as jwt;
//use wasm_bindgen::prelude::*;
use juniper::http::GraphQLRequest;
use serde_json;

pub use crate::dal::DynaDB;
pub use crate::schema::{create_schema, Schema};

#[macro_use]
mod macros;
mod schema;
pub mod dal;
mod security;
pub mod authentication;
mod validation;

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

    static GET_THINGS_HHHH_QUERY: &'static str = include_str!("get_things_hhhh_query.graphql");

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn test_query_execution() {

        let gql_response = execute_query(
            format!("{{\"query\": \"{}\"}}", GET_THINGS_HHHH_QUERY.to_string())
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
