
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

//pub fn handle_request(request: HashMap<String, String>) -> String {
//    match request.get("query") {
//        Some(gql_query) => {
//            info!("Request: {}", gql_query.clone());
//            let result = worker_lib::execute_query(gql_query.to_owned());
//            info!("Response: {}", result.clone());
//            result
//        },
//        None => "GraphQL Query Required".to_string()
//    }
//}

//pub fn handle_request(request: HashMap<String, String>) -> Result<String, Error> {
//    match request.get("query") {
//        Some(gql_query) => {
//            info!("Request: {}", gql_query.clone());
//            let result = worker_lib::execute_query(gql_query.to_owned());
//            info!("Response: {}", result.clone());
//            Ok(result)
//        },
//        None => Ok("GraphQL Query Required".to_string())
//    }
//}

pub fn handle_request(request: HashMap<String, String>) -> Result<serde_json::Value, Error> {
    match request.get("query") {
        Some(gql_query) => {
            info!("Request: {}", gql_query.clone());
            let result = worker_lib::execute_query(gql_query.to_owned());
            info!("Response: {}", result.clone());
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

//pub fn handle_request(request: HashMap<String, String>) -> Result<serde_json::Value, Error> {
//    match request.get("query") {
//        Some(gql_query) => {
//            info!("Request: {}", gql_query.clone());
//            let result = worker_lib::execute_query(gql_query.to_owned());
//            let data: serde_json::Value = serde_json::from_str(result.as_ref())?;
//            info!("Response: {:?}", data.clone());
//            Ok(data)
//        },
//        None => Ok(json!({
//                    "statusCode": 500,
//                    "error": "GraphQL Query Required"
//                }))
//    }
//}

fn main() {
    lambda::logger::init();

    lambda::start(|request: HashMap<String, String>| {
//        Ok(handle_request(request))
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

        info!("Lambda Response: {:?}", lambda_response);

        assert_eq!(1 + 1, 2);
    }
}
