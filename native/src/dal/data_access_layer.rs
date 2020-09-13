use futures::future::join_all;

use crate::dal::models::{
    DocumentRef, Document, DocStreamRef, DocStream
};
use crate::dal::{ScanParams, QueryParams};

#[derive(Debug)]
pub enum ContentListData{
    Document {data: Option<Vec<Document>>},
    DocStream {data: Option<Vec<DocStream>>}
}

#[derive(Debug)]
pub enum QueryData{
    Document {data: QueryParams},
    DocStream {data: QueryParams}
}

#[derive(Debug)]
pub enum ScanData{
    Document {data: ScanParams},
    DocStream {data: ScanParams}
}

#[derive(Debug)]
pub enum ContentRef{
    Doc (DocumentRef),
    Stream (DocStreamRef)
}

pub trait ContentRefDocOrStream {
    fn as_content_ref(self) -> ContentRef;
    fn get_value(self) -> Box<dyn ContentRefDocOrStream>;
}

impl ContentRefDocOrStream for DocumentRef {
    fn as_content_ref(self) -> ContentRef {
        ContentRef::Doc(self)
    }

    fn get_value(self) -> Box<dyn ContentRefDocOrStream> {
        Box::new(self)
    }
}

impl ContentRefDocOrStream for DocStreamRef {
    fn as_content_ref(self) -> ContentRef {
        ContentRef::Stream(self)
    }

    fn get_value(self) -> Box<dyn ContentRefDocOrStream> {
        Box::new(self)
    }
}

#[derive(Debug)]
pub enum Content {
    Doc(Document),
    Stream(DocStream)
}

pub trait ContentDocOrStream {
    fn as_content(self) -> Content;
    fn get_value(self) -> Box<dyn ContentDocOrStream>;
}

impl ContentDocOrStream for Document {
    fn as_content(self) -> Content {
        Content::Doc(self)
    }

    fn get_value(self) -> Box<dyn ContentDocOrStream> {
        Box::new(self)
    }
}

impl ContentDocOrStream for DocStream {
    fn as_content(self) -> Content {
        Content::Stream(self)
    }

    fn get_value(self) -> Box<dyn ContentDocOrStream> {
        Box::new(self)
    }
}

pub async fn query(payload: QueryData) -> ContentListData {
    match payload {
        QueryData::Document {data} => {
            let docs = Document::query(data).await;
            ContentListData::Document {data: docs}
        },
        QueryData::DocStream {data} => {
            let docs = DocStream::query(data).await;
            ContentListData::DocStream {data: docs}
        }
    }
}

// pub async fn scan(payload: ScanParams) -> Option<Vec<Content>> {
//     let mut handles = vec![];
//     handles.push(
//         Document::scan(payload.clone())
//     );
//     handles.push(
//         DocStream::scan(payload)
//     );
//
//     let res = join_all(handles).await;
//
//     let content_list = res.into_iter().filter(|c| {
//         c.is_some()
//     }).map(|x| x.unwrap().as_content()).collect();
//
//     content_list
// }

pub async fn scan(payload: ScanData) -> ContentListData {
    match payload {
        ScanData::Document {data} => {
            let docs = Document::scan(data).await;
            ContentListData::Document {data: docs}
        },
        ScanData::DocStream {data} => {
            let docs = DocStream::scan(data).await;
            ContentListData::DocStream {data: docs}
        }
    }
}

pub async fn get<T: ContentDocOrStream>(payload: S) -> Option<dyn ContentRefDocOrStream> {
    match payload.as_content_ref() {
        ContentRef::Doc(r) => Document::get(r).await,
        ContentRef::Stream(r) => DocStream::get(r).await
    }
}

pub async fn create<T: ContentDocOrStream>(payload: T) -> Option<dyn ContentRefDocOrStream> {
    match payload.as_content() {
        Content::Doc(d) => Document::create(d).await,
        Content::Stream(s) => DocStream::create(s).await
    }
}

pub async fn update<T: ContentDocOrStream>(payload: T) -> Option<dyn ContentRefDocOrStream> {
    match payload.as_content() {
        Content::Doc(d) => Document::update(d).await,
        Content::Stream(s) => DocStream::update(s).await
    }
}

pub async fn delete(payload: S) -> Option<dyn ContentRefDocOrStream> {
    match payload.as_content_ref() {
        ContentRef::Doc(r) => Document::delete(r).await,
        ContentRef::Stream(r) => DocStream::delete(r).await
    }
}
