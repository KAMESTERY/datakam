
//#![feature(use_extern_macros, specialization)]
//#![feature(specialization)] // for 0.4

//extern crate pyo3;
extern crate log;
extern crate worker_lib;

//use pyo3::prelude::*;

//#[pyfunction]
//pub fn handle_request(request: String) -> PyResult<String> {
//    let result = worker_lib::execute_query(request);
//    Ok(result)
//}

#[no_mangle]
pub extern "C" fn handle_request(request: String) -> String {
    let result = worker_lib::execute_query(request);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    static get_things_hhhh_query: &'static str = include_str!("get_things_hhhh_query.graphql");

    #[test]
    fn test_query_execution() {

        let request = format!("{{\"query\": \"{}\"}}", get_things_hhhh_query.to_string());

//        let lambda_response = handle_request(request);
        let lambda_response = unsafe {
            handle_request(request)
        };

        println!("Lambda Response: {:?}", lambda_response);

        assert_eq!(1 + 1, 2);
    }
}
