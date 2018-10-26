
#[macro_use]
extern crate log;
extern crate protobuf;
extern crate grpc;
extern crate futures;
extern crate futures_cpupool;
extern crate worker_lib;

use worker_lib::dal::{UserAuthData};
use worker_lib::authentication as auth;

mod api;

use api::{
    UserCredentialsReq, AuthClaimsResp,
    UserEnrollReq, EnrollStatusResp,
    AuthKam, AuthKamClient, AuthKamServer
};

struct AuthKamImpl;

impl AuthKam for AuthKamImpl {
    fn authenticate(&self, o: grpc::RequestOptions, p: UserCredentialsReq) -> grpc::SingleResponse<AuthClaimsResp> {
        let mut acr = AuthClaimsResp::new();

        let token = auth::login(p.email.clone(), p.email, p.password).unwrap_or("".to_string());
        let claims = auth::get_claims(token.clone()).unwrap_or_default();

        acr.token = token;
        acr.userid = claims.user_id.unwrap_or("".to_string());
        acr.email = claims.email.unwrap_or("".to_string());
        acr.role = claims.role.unwrap_or_default();

        grpc::SingleResponse::completed(acr)
    }

    fn enroll(&self, o: grpc::RequestOptions, p: UserEnrollReq) -> grpc::SingleResponse<EnrollStatusResp> {
        unimplemented!()
    }
}

fn main() {
    println!("Hello, world!");
}
