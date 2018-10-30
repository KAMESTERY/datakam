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

pub trait ContentKam {
    fn create(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles>;

    fn update(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles>;

    fn get(&self, o: ::grpc::RequestOptions, p: super::contentkm::ContentHandles) -> ::grpc::SingleResponse<super::contentkm::Content>;

    fn delete(&self, o: ::grpc::RequestOptions, p: super::contentkm::ContentHandles) -> ::grpc::SingleResponse<super::contentkm::ContentHandles>;
}

// client

pub struct ContentKamClient {
    grpc_client: ::std::sync::Arc<::grpc::Client>,
    method_Create: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::contentkm::Content, super::contentkm::ContentHandles>>,
    method_Update: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::contentkm::Content, super::contentkm::ContentHandles>>,
    method_Get: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::contentkm::ContentHandles, super::contentkm::Content>>,
    method_Delete: ::std::sync::Arc<::grpc::rt::MethodDescriptor<super::contentkm::ContentHandles, super::contentkm::ContentHandles>>,
}

impl ::grpc::ClientStub for ContentKamClient {
    fn with_client(grpc_client: ::std::sync::Arc<::grpc::Client>) -> Self {
        ContentKamClient {
            grpc_client: grpc_client,
            method_Create: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/grpc.ContentKam/Create".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
            method_Update: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/grpc.ContentKam/Update".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
            method_Get: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/grpc.ContentKam/Get".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
            method_Delete: ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                name: "/grpc.ContentKam/Delete".to_string(),
                streaming: ::grpc::rt::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
        }
    }
}

impl ContentKam for ContentKamClient {
    fn create(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles> {
        self.grpc_client.call_unary(o, p, self.method_Create.clone())
    }

    fn update(&self, o: ::grpc::RequestOptions, p: super::contentkm::Content) -> ::grpc::SingleResponse<super::contentkm::ContentHandles> {
        self.grpc_client.call_unary(o, p, self.method_Update.clone())
    }

    fn get(&self, o: ::grpc::RequestOptions, p: super::contentkm::ContentHandles) -> ::grpc::SingleResponse<super::contentkm::Content> {
        self.grpc_client.call_unary(o, p, self.method_Get.clone())
    }

    fn delete(&self, o: ::grpc::RequestOptions, p: super::contentkm::ContentHandles) -> ::grpc::SingleResponse<super::contentkm::ContentHandles> {
        self.grpc_client.call_unary(o, p, self.method_Delete.clone())
    }
}

// server

pub struct ContentKamServer;


impl ContentKamServer {
    pub fn new_service_def<H : ContentKam + 'static + Sync + Send + 'static>(handler: H) -> ::grpc::rt::ServerServiceDefinition {
        let handler_arc = ::std::sync::Arc::new(handler);
        ::grpc::rt::ServerServiceDefinition::new("/grpc.ContentKam",
            vec![
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/grpc.ContentKam/Create".to_string(),
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
                        name: "/grpc.ContentKam/Update".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerUnary::new(move |o, p| handler_copy.update(o, p))
                    },
                ),
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/grpc.ContentKam/Get".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerUnary::new(move |o, p| handler_copy.get(o, p))
                    },
                ),
                ::grpc::rt::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::rt::MethodDescriptor {
                        name: "/grpc.ContentKam/Delete".to_string(),
                        streaming: ::grpc::rt::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::rt::MethodHandlerUnary::new(move |o, p| handler_copy.delete(o, p))
                    },
                ),
            ],
        )
    }
}
