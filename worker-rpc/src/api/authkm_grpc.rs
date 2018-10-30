// This file is generated. Do not edit
// @generated

// https://github.com/Manishearth/rust-clippy/issues/702
#![allow(unknown_lints)]
#![allow(clippy)]

#![cfg_attr(rustfmt, rustfmt_skip)]

#![allow(box_pointers)]
#![allow(dead_code)]
#![allow(missing_docs)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(trivial_casts)]
#![allow(unsafe_code)]
#![allow(unused_imports)]
#![allow(unused_results)]


// interface

pub trait AuthKam {
    fn authenticate(&self, o: ::grpc::RequestOptions, p: super::authkm::UserCredentialsReq) -> ::grpc::SingleResponse<super::authkm::AuthClaimsResp>;

    fn enroll(&self, o: ::grpc::RequestOptions, p: super::authkm::UserEnrollReq) -> ::grpc::SingleResponse<super::authkm::EnrollStatusResp>;
}

// client

pub struct AuthKamClient {
    grpc_client: ::std::sync::Arc<::grpc::Client>,
    method_Authenticate: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::authkm::UserCredentialsReq, super::authkm::AuthClaimsResp>>,
    method_Enroll: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::authkm::UserEnrollReq, super::authkm::EnrollStatusResp>>,
}

impl ::grpc::ClientStub for AuthKamClient {
    fn with_client(grpc_client: ::std::sync::Arc<::grpc::Client>) -> Self {
        AuthKamClient {
            grpc_client: grpc_client,
            method_Authenticate: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/grpc.AuthKam/Authenticate".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
            method_Enroll: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/grpc.AuthKam/Enroll".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
        }
    }
}

impl AuthKam for AuthKamClient {
    fn authenticate(&self, o: ::grpc::RequestOptions, p: super::authkm::UserCredentialsReq) -> ::grpc::SingleResponse<super::authkm::AuthClaimsResp> {
        self.grpc_client.call_unary(o, p, self.method_Authenticate.clone())
    }

    fn enroll(&self, o: ::grpc::RequestOptions, p: super::authkm::UserEnrollReq) -> ::grpc::SingleResponse<super::authkm::EnrollStatusResp> {
        self.grpc_client.call_unary(o, p, self.method_Enroll.clone())
    }
}

// server

pub struct AuthKamServer;


impl AuthKamServer {
    pub fn new_service_def<H : AuthKam + 'static + Sync + Send + 'static>(handler: H) -> ::grpc::rt::ServerServiceDefinition {
        let handler_arc = ::std::sync::Arc::new(handler);
        ::grpc::rt::ServerServiceDefinition::new("/grpc.AuthKam",
            vec![
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/grpc.AuthKam/Authenticate".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerUnary::new(move |o, p| handler_copy.authenticate(o, p))
                    },
                ),
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/grpc.AuthKam/Enroll".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerUnary::new(move |o, p| handler_copy.enroll(o, p))
                    },
                ),
            ],
        )
    }
}
