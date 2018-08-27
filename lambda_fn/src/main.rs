
extern crate lib_api;
extern crate aws_lambda as lambda;
#[macro_use] extern crate log;

fn main() {
    lambda::logger::init();

    info!(
        "starting function {} (version {}) [{} bytes]",
        lambda::env::function_name(),
        lambda::env::function_version(),
        lambda::env::function_memory_size()
    );

    lambda::start(|input: String| {

        debug!("Input: {}", input);

        let response = unsafe {
            lib_api::execute_query(input)
        };

        Ok(response)
    });
}
