#![type_length_limit="2597059"]

#[macro_use]
extern crate log;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate neon;
#[macro_use]
extern crate neon_serde;

use jsonwebtoken as jwt;
// use serde_json;
use neon::prelude::*;

#[macro_use]
mod macros;
mod dal;
mod security;
mod authentication;
mod validation;
mod bridge;

// fn hello(mut cx: FunctionContext) -> JsResult<JsString> {
//     Ok(cx.string("hello node"))
// }
//
// register_module!(mut cx, {
//     // cx.export_function("enroll", bridge::enroll);
//     cx.export_function("createDocument", bridge::create_document);
//     cx.export_function("deleteDocument", bridge::delete_document);
//     cx.export_function("createMedia", bridge::create_media);
//     cx.export_function("deleteMedia", bridge::delete_media);
//     cx.export_function("authenticate", bridge::authenticate);
//     cx.export_function("getUserinfo", bridge::get_userinfo);
//     cx.export_function("getDocumentAndRelated", bridge::get_document_and_related);
//     cx.export_function("getDocumentByTopics", bridge::get_documents_by_topics);
//     cx.export_function("hello", hello);
//     Ok(())
// });
