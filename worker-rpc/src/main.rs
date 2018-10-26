
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate protobuf;
extern crate grpc;
extern crate tls_api;
extern crate tls_api_native_tls;
extern crate futures;
extern crate futures_cpupool;
extern crate num_cpus;
extern crate worker_lib;
extern crate openssl_probe;

use std::env;
use std::thread;

use tls_api::TlsAcceptorBuilder;

mod api;
mod services;

use api::{
    AuthKamServer
};
use services::{
    AuthKamImpl
};

fn main() {
    openssl_probe::init_ssl_cert_env_vars();

    match env::var("RUST_LOG").ok() {
        Some(rust_log) => env::set_var("RUST_LOG", rust_log),
        None => env::set_var("RUST_LOG", "protobuf,grpc,worker_lib,worker_rpc=debug")
    }
    env_logger::init();

    let port = env::var("PORT")
        .unwrap_or_else(|_| "9099".to_string()).parse::<u16>().unwrap_or(9099);
    let thread_count = num_cpus::get() * 2 + 1; // Choose just 3 by default ?

//    let mut server = grpc::ServerBuilder::new();
    let mut server = grpc::ServerBuilder::new_plain();
    server.http.set_port(port.clone());
    server.add_service(AuthKamServer::new_service_def(AuthKamImpl));
    server.http.set_cpu_pool_threads(thread_count);

    let _server = server.build().expect("server");

    info!("Server started on PORT: {}", port);

    loop {
        thread::park()
    }
}
