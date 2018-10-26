
use std::sync::Arc;

use grpc;
use grpc::{ClientStub, ClientStubExt};

use api::{
    UserCredentialsReq, AuthClaimsResp,
    UserEnrollReq, EnrollStatusResp,
    AuthKam, AuthKamClient
};

#[test]
fn test_enroll() {
    let port = 9099;

    let auth_kam_client = AuthKamClient::with_client(
        Arc::new(
            grpc::Client::new_plain(
                "::1",
                port,
                Default::default()
            ).unwrap()
        )
    );

    let mut reg_req = UserEnrollReq::new();
    reg_req.username = "gggg".to_string();
    reg_req.email = "gggg@gggg.ggg".to_string();
    reg_req.password = "gggggggg".to_string();

    let (meta1, enroll_status, meta2) = auth_kam_client.enroll(grpc::RequestOptions::new(), reg_req).wait().unwrap();
    println!("Enrollment Status MetaData1:::: {:?}", meta1);
    println!("Enrollment Status Response:::: {:?}", enroll_status);
    println!("Enrollment Status MetaData2:::: {:?}", meta2);

    assert_eq!(1, 1);
}

#[test]
fn test_authenticate() {
    let port = 9099;

    let auth_kam_client = AuthKamClient::with_client(
        Arc::new(
            grpc::Client::new_plain(
                "::1",
                port,
                Default::default()
            ).unwrap()
        )
    );

    let mut auth_req = UserCredentialsReq::new();
    auth_req.email = "gggg@gggg.ggg".to_string();
    auth_req.password = "gggggggg".to_string();

    let (meta1, auth_resp, meta2) = auth_kam_client.authenticate(grpc::RequestOptions::new(), auth_req).wait().unwrap();
    println!("AuthRentication Claims MetaData1:::: {:?}", meta1);
    println!("AuthRentication Claims Response:::: {:?}", auth_resp);
    println!("AuthRentication Claims MetaData2:::: {:?}", meta2);

    assert_eq!(1, 1);
}
