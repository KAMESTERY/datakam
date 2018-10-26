
use grpc;

use worker_lib::dal::{UserAuthData};
use worker_lib::authentication as auth;

use api::{
    UserCredentialsReq, AuthClaimsResp,
    UserEnrollReq, EnrollStatusResp,
    AuthKam
};

pub struct AuthKamImpl;

impl AuthKam for AuthKamImpl {
    fn authenticate(&self, o: grpc::RequestOptions, p: UserCredentialsReq) -> grpc::SingleResponse<AuthClaimsResp> {
        let mut acr = AuthClaimsResp::new();

        let token = auth::login(p.email.clone(), p.email, p.password).unwrap_or("".to_string());
        let claims = auth::get_claims(token.clone()).unwrap_or_default();

        acr.token = token;
        acr.user_id = claims.user_id.unwrap_or("".to_string());
        acr.email = claims.email.unwrap_or("".to_string());
        acr.role = claims.role.unwrap_or_default();

        grpc::SingleResponse::completed(acr)
    }

    fn enroll(&self, o: grpc::RequestOptions, p: UserEnrollReq) -> grpc::SingleResponse<EnrollStatusResp> {
        let mut esr = EnrollStatusResp::new();

        let status = auth::register(p.email.clone(), p.email, p.username, p.password);
        let success = !status.is_empty();

        esr.success = success;
        esr.message = status;

        grpc::SingleResponse::completed(esr)
    }
}
