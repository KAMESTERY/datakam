
#[macro_use]
extern crate log;
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

use futures::future::Future;
use juniper::http::{GraphQLRequest, GraphQLResponse};

mod schema;
mod dal;
mod security;
mod authentication;
mod validation;

use schema::create_schema;
use dal::{DynaDB};

#[no_mangle]
pub unsafe extern "C" fn execute_query(query: String) -> Option<String> {

    debug!("GQL Query: {}", query.clone());

    let gql_request: GraphQLRequest = serde_json::from_str(query.as_ref()).unwrap();
    debug!("GQL Request: {:?}", gql_request.clone());

    let schema = create_schema();
    let result = gql_request.execute(&schema, &());

    let json_string = serde_json::to_string(&result).ok();

    json_string
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

        let gql_response = unsafe {
            execute_query(
                format!("{{\"query\": \"{}\"}}", get_things_hhhh_query.to_string())
            )
        };

        debug!("GQL Response: {}", gql_response.clone().unwrap());

        assert!(gql_response.is_some());
    }
}
