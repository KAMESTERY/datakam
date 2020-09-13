use crate::dal::{
    User, UserAuthData, UserGroup, UserProfile, DocumentInput, QueryInput,
    query, scan, get, create, update, delete,
    Content, Document, DocStream, ContentDocOrStream,
    ContentRef, DocumentRef, DocStreamRef, ContentRefDocOrStream};

use futures::future::join_all;

use neon::prelude::*;
use rayon::prelude::*;
use std::sync::Arc;
use std::collections::HashMap;
use tokio::runtime::Runtime;
use serde::de::Error;

// pub fn create_media(mut cx: FunctionContext) -> JsResult<JsArray> {
//     let media_obj = cx.argument::<JsObject>(0)?
//         .downcast::<JsObject>()
//         .unwrap_or(JsObject::new(&mut cx));
//
//     let name = get_js_val!(media_obj, cx, "ParentDocumentID", JsString, cx.string(""));
//     let user_id = get_js_val!(media_obj, cx, "UserID", JsString, cx.string(""));
//     let media_id = get_js_val!(media_obj, cx, "MediaID", JsString, cx.string(""));
//     let file_url = get_js_val!(media_obj, cx, "FileUrl", JsString, cx.string(""));
//     let data = vec![
//         vec!["FileUrl".to_owned(), file_url]
//     ];
//     let tags_obj = media_obj.get(&mut cx, "Tags")?.downcast::<JsArray>().unwrap_or(cx.empty_array());
//     let tags: Vec<String> = get_js_array!(tags_obj, cx, JsString, cx.string(""))
//         .filter(|tag| {
//             !tag.is_empty()
//         }).collect();
//     let media_with_parent = MediaInput {
//         name: name.clone(),
//         user_id: user_id.clone(),
//         media_id: media_id.clone(),
//         data: data.clone(),
//         tags: Some(tags.clone()),
//     };
//     let media_with_type = MediaInput {
//         name,
//         user_id,
//         media_id,
//         data,
//         tags: Some(tags),
//     };
//
//     let input_data = InputData::Media {
//         media: vec![media_with_parent, media_with_type]
//     };
//
//     let mut rt = Runtime::new().unwrap();
//     let ids = rt.block_on(async move {
//         create(input_data).await
//     });
//
//     let js_ids = JsArray::new(&mut cx, ids.len() as u32);
//     ids.iter().enumerate().for_each(|item| {
//         let (idx, id) = item;
//         let id_str = cx.string(id);
//         js_ids.set(&mut cx, idx as u32, id_str);
//     });
//
//     Ok(js_ids)
// }
//
// pub fn delete_media(mut cx: FunctionContext) -> JsResult<JsArray> {
//     let media_obj = cx.argument::<JsObject>(0)?
//         .downcast::<JsObject>()
//         .unwrap_or(JsObject::new(&mut cx));
//
//     let name = get_js_val!(media_obj, cx, "Name", JsString, cx.string(""));
//     let thing_id = get_js_val!(media_obj, cx, "ThingID", JsString, cx.string(""));
//     let data = vec![
//         vec![name, thing_id]
//     ];
//
//     let delete_media = DeleteData::Media {
//         data
//     };
//
//     let mut rt = Runtime::new().unwrap();
//     let ids = rt.block_on(async move {
//         delete(delete_media).await
//     });
//
//     let js_ids = JsArray::new(&mut cx, ids.len() as u32);
//     ids.iter().enumerate().for_each(|item| {
//         let (idx, id) = item;
//         let id_str = cx.string(id);
//         js_ids.set(&mut cx, idx as u32, id_str);
//     });
//
//     Ok(js_ids)
// }

export! {
    fn create_document(doc: Document) -> Option<ContentRef> {
        dbg!(&doc);

        let mut rt = Runtime::new().unwrap();
        let content_ref = rt.block_on(async move {
            create(doc).await
        });

        content_ref
    }

    fn delete_document(doc_ref: DocumentRef) -> Option<DocumentRef> {
        dbg!(&doc_ref);

        let mut rt = Runtime::new().unwrap();
        let content_ref = rt.block_on(async move {
            delete(doc_ref).await
        });

        content_ref
    }

    fn get_document_and_related(doc_ref: DocumentRef) -> Option<String> {
        dbg!(&doc_ref);

        let mut rt = Runtime::new().unwrap();
        let content = rt.block_on(async move {
            get(doc_ref).await
        });

        serde_json::to_string(*content.get_value()).ok()
    }
}

// pub fn get_documents_by_topics(mut cx: FunctionContext) -> JsResult<JsArray> {
//     let titles_obj = cx.argument::<JsArray>(0)?;
//
//     let topics: Vec<String> = get_js_array!(titles_obj, cx, JsString, cx.string(""))
//         .filter(|topic| {
//             !topic.is_empty()
//         }).collect();
//
//     let mut rt = Runtime::new().unwrap();
//     let mut handles = vec![];
//
//     for topic in topics {
//         handles.push(
//             rt.spawn(async move {
//                 let queryInput = QueryInput {
//                     attr_names: Some(vec![vec![String::from("#n"), String::from("Name")]]),
//                     key_condition_expr: Some(String::from("#n = :cat")),
//                     raw_data: Some(vec![vec![String::from(":cat"), topic]]),
//                     ..QueryInput::default()
//                 };
//                 query(vec![queryInput]).await
//             })
//         );
//     }
//
//     rt.block_on(async move {
//         let document_handles = join_all(handles).await;
//         let docs_by_topic = JsArray::new(&mut cx, document_handles.len() as u32);
//
//         let mut i: u32 = 0;
//         for document_handle in document_handles {
//             let docs = document_handle.unwrap();
//             if !docs.is_empty() {
//                 let object = JsObject::new(&mut cx);
//                 let topic = cx.string(docs.first().unwrap().thing.name.clone().unwrap_or_default());
//                 object.set(&mut cx, "Topic", topic).unwrap();
//                 let docs_array = JsArray::new(&mut cx, docs.len() as u32);
//                 for (j, m) in docs.iter().enumerate() {
//                     let docs_object = JsObject::new(&mut cx);
//                     to_js_object(&mut cx, docs_object, m.to_owned());
//                     docs_array.set(&mut cx, j as u32, docs_object).unwrap();
//                 }
//                 object.set(&mut cx, "Docs", docs_array).unwrap();
//                 docs_by_topic.set(&mut cx, i, object);
//                 i += 1;
//             }
//         }
//
//         info!("Returning A JS Array");
//
//         Ok(docs_by_topic)
//     })
// }
//
// pub fn get_document_and_related(mut cx: FunctionContext) -> JsResult<JsObject> {
//     let doc_ref_obj = cx.argument::<JsObject>(0)?
//         .downcast::<JsObject>()
//         .unwrap_or(JsObject::new(&mut cx));
//
//     let topic = get_js_val!(doc_ref_obj, cx, "Topic", JsString, cx.string(""));
//     let title = get_js_val!(doc_ref_obj, cx, "Title", JsString, cx.string(""));
//
//     let topic_ref = Arc::new(topic);
//     let title_ref = Arc::new(title);
//
//     let mut rt = Runtime::new().unwrap();
//
//     let topic = topic_ref.clone();
//     let title = title_ref.clone();
//
//     let document_handle = rt.spawn(async move {
//         get(vec![vec![topic.to_string(), title.to_string()]]).await
//     });
//
//     let title = title_ref.clone();
//
//     let media_handle = rt.spawn(async move {
//         let queryInput = QueryInput {
//             attr_names: Some(vec![vec![String::from("#n"), String::from("Name")]]),
//             key_condition_expr: Some(String::from("#n = :cat")),
//             raw_data: Some(vec![vec![String::from(":cat"), title.to_string()]]),
//             ..QueryInput::default()
//         };
//         query(vec![queryInput]).await
//     });
//
//     let topic = topic_ref.clone();
//
//     let related_handle = rt.spawn(async move {
//         let queryInput = QueryInput {
//             attr_names: Some(vec![vec![String::from("#n"), String::from("Name")]]),
//             key_condition_expr: Some(String::from("#n = :cat")),
//             raw_data: Some(vec![vec![String::from(":cat"), topic.to_string()]]),
//             ..QueryInput::default()
//         };
//         query(vec![queryInput]).await
//     });
//
//     rt.block_on(async move {
//         let object = JsObject::new(&mut cx);
//
//         let doc_opt = document_handle.await;
//         if doc_opt.is_ok() {
//             match doc_opt.unwrap() {
//                 Some(doc_vec) => match doc_vec.first() {
//                     Some(doc) => to_js_object(&mut cx, object, doc.to_owned()),
//                     None => ()
//                 }
//                 None => ()
//             }
//         }
//
//         let media_res = media_handle.await;
//         if media_res.is_ok() {
//             let media = media_res.unwrap();
//             if !media.is_empty() {
//                 let media_array = JsArray::new(&mut cx, media.len() as u32);
//                 for (i, m) in media.iter().enumerate() {
//                     let media_object = JsObject::new(&mut cx);
//                     to_js_object(&mut cx, media_object, m.to_owned());
//                     media_array.set(&mut cx, i as u32, media_object).unwrap();
//                 }
//                 object.set(&mut cx, "Media", media_array).unwrap();
//             }
//         }
//
//         let related_res = related_handle.await;
//         if related_res.is_ok() {
//             let related = related_res.unwrap();
//             if !related.is_empty() {
//                 let related_array = JsArray::new(&mut cx, related.len() as u32);
//                 for (i, m) in related.iter().enumerate() {
//                     let related_object = JsObject::new(&mut cx);
//                     to_js_object(&mut cx, related_object, m.to_owned());
//                     related_array.set(&mut cx, i as u32, related_object).unwrap();
//                 }
//                 object.set(&mut cx, "Related", related_array).unwrap();
//             }
//         }
//
//         Ok(object)
//     })
// }
//
// fn to_js_object(cx: &mut FunctionContext, object: Handle<JsObject>, to: ThingOutput) {
//     dbg!(&to);
//
//     let topic = cx.string(to.thing.name.unwrap_or_default());
//     let thing_id = cx.string(to.thing.thing_id.clone().unwrap_or_default());
//     let user_id = cx.string(to.thing.user_id.unwrap_or_default());
//     let identifier = cx.string(to.thing.thing_id.unwrap_or_default());
//     let score = cx.number(to.thing.score.unwrap_or_default());
//     let version = cx.number(to.thing.version.unwrap_or_default());
//     let created_at = cx.string(to.thing.created_at.unwrap_or_default());
//     let updated_at = cx.string(to.thing.updated_at.unwrap_or_default());
//
//     let tags = to.thing.tags.unwrap_or_default();
//     let tags_array = JsArray::new(cx, tags.len() as u32);
//     for (i, tag) in tags.iter().enumerate() {
//         let tag_string = cx.string(tag);
//         tags_array.set(cx, i as u32, tag_string).unwrap();
//     }
//
//     object.set(cx, "Topic", topic).unwrap();
//     object.set(cx, "DocumentID", thing_id).unwrap();
//     object.set(cx, "UserID", user_id).unwrap();
//     object.set(cx, "Identifier", identifier).unwrap();
//     object.set(cx, "Score", score).unwrap();
//     object.set(cx, "Version", version).unwrap();
//     object.set(cx, "Tags", tags_array).unwrap();
//     object.set(cx, "CreatedAt", created_at).unwrap();
//     object.set(cx, "UpdatedAt", updated_at).unwrap();
//
//     for data in to.data {
//         let key = data.key.unwrap_or_default();
//         let val = cx.string(data.value.unwrap_or_default());
//
//         if  // Document Attributes
//         (key == "Slug") ||
//             (key == "Publish") ||
//             (key == "Media") ||
//             (key == "Tags") ||
//             (key == "FiltreVisuel") ||
//             (key == "Niveau") ||
//             (key == "Langue") ||
//             (key == "Title") ||
//             (key == "Body") ||
//             // Media Attributes
//             (key == "Type") ||
//             (key == "MediaID") ||
//             (key == "ParentDocumentID") ||
//             (key == "FileUrl") {
//             object.set(cx, key.as_ref(), val).unwrap();
//         }
//     }
// }

// pub fn query_documents_by_topics_and_tags(topics: Vec<String>, tags: Vec<String>) -> JsResult<JsArray> {
//
// }
