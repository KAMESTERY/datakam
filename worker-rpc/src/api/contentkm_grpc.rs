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
    fn create(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles>;

    fn update(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles>;
}

// client

pub struct AuthKamClient {
    grpc_client: ::std::sync::Arc<::grpc::Client>,
    method_Create: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::contentkm::Content, super::contentkm::ContentHandles>>,
    method_Update: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::contentkm::Content, super::contentkm::ContentHandles>>,
}

impl ::grpc::ClientStub for AuthKamClient {
    fn with_client(grpc_client: ::std::sync::Arc<::grpc::Client>) -> Self {
        AuthKamClient {
            grpc_client: grpc_client,
            method_Create: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/contentkm.AuthKam/Create".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
            method_Update: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/contentkm.AuthKam/Update".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
        }
    }
}

impl AuthKam for AuthKamClient {
    fn create(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles> {
        self.grpc_client.call_unary(o, p, self.method_Create.clone())
    }

    fn update(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles> {
        self.grpc_client.call_unary(o, p, self.method_Update.clone())
    }
}

// server

pub struct AuthKamServer;


impl AuthKamServer {
    pub fn new_service_def<H : AuthKam + 'static + Sync + Send + 'static>(handler: H) -> ::grpc::rt::ServerServiceDefinition {
        let handler_arc = ::std::sync::Arc::new(handler);
        ::grpc::rt::ServerServiceDefinition::new("/contentkm.AuthKam",
            vec![
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/contentkm.AuthKam/Create".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerUnary::new(move |o, p| handler_copy.create(o, p))
                    },
                ),
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/contentkm.AuthKam/Update".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerUnary::new(move |o, p| handler_copy.update(o, p))
                    },
                ),
            ],
        )
    }
}
