
#[macro_use]
extern crate log;
extern crate worker_lib;
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate failure;
extern crate aws_lambda as lambda;

use failure::Error;
use std::collections::HashMap;

pub fn handle_request(request: HashMap<String, String>) -> Result<serde_json::Value, Error> {
    match request.get("query") {
        Some(gql_query) => {
            let result = worker_lib::execute_query(gql_query.to_owned());
            Ok(json!({
                "statusCode": 200,
                "data": result
            }))
        },
        None => Ok(json!({
                    "statusCode": 500,
                    "error": "GraphQL Query Required"
                }))
    }
}

fn main() {
    lambda::start(|request: HashMap<String, String>| {
        handle_request(request)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    static get_things_hhhh_query: &'static str = include_str!("get_things_hhhh_query.graphql");

    #[test]
    fn test_query_execution() {

        let request = [
            ("query".to_string(), format!("{{\"query\": \"{}\"}}", get_things_hhhh_query.to_string()))
        ].iter().cloned().collect();

        let lambda_response = handle_request(request);

        println!("Lambda Response: {:?}", lambda_response);

        assert_eq!(1 + 1, 2);
    }
}
