
use grpc;

use worker_lib::dal::{UserAuthData};
use worker_lib::authentication as auth;

use api::{
    Content, ContentHandles,
    ContentKam
};

pub struct ContentKamImpl;

impl ContentKam for ContentKamImpl {
    fn create(&self, o: grpc::RequestOptions, p: Content) -> grpc::SingleResponse<ContentHandles> {
        unimplemented!()
    }

    fn update(&self, o: grpc::RequestOptions, p: Content) -> grpc::SingleResponse<ContentHandles> {
        unimplemented!()
    }

    fn get(&self, o: grpc::RequestOptions, p: ContentHandles) -> grpc::SingleResponse<Content> {
        unimplemented!()
    }

    fn delete(&self, o: grpc::RequestOptions, p: ContentHandles) -> grpc::SingleResponse<ContentHandles> {
        unimplemented!()
    }
}
