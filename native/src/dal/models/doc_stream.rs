use std::collections::HashMap;

use rusoto_dynamodb::AttributeValue;

use crate::dal::dynamodb::{
    attr_n, attr_s, attr_ss, attr_bool,
    DynaDB, QueryParams, ScanParams
};

use crate::dal::dynatraits::ModelDynaConv;
use crate::dal::models::constants::*;
use crate::dal::models::media::{Media};
use crate::dal::models::text_block::{TextBlock};

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct DocStream {
    pub namespace: Option<String>,
    pub content_id: Option<String>,
    pub user_id: Option<String>,
    pub score: Option<i32>,
    pub version: Option<i32>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
    pub tags: Option<Vec<String>>,
    pub media: Option<Vec<Media>>,
    pub text_blocks: Option<Vec<TextBlock>>,
}

impl DocStream {
    // API Functions
    pub async fn query(query_params: QueryParams) -> Option<Vec<DocStream>> {
        let docs = DynaDB::query(query_params).await;
        docs
    }

    pub async fn scan(scan_params: ScanParams) -> Option<Vec<DocStream>> {
        let docs = DynaDB::scan(scan_params).await;
        docs
    }

    pub async fn get(content_ref: DocStreamRef) -> Option<DocStream> {
        let doc_key = DocStream {
            namespace: content_ref.clone().namespace,
            content_id: content_ref.clone().content_id,
            ..DocStream::default()
        }.key();

        let media_keys = vec![
            Media {
                parent_document_id: content_ref.clone().content_id,
                ..Media::default()
            }.key()
        ];
        let media = DynaDB::batchget_table(CONTENT_TABLE.into(), media_keys);

        let text_block_keys = vec![
            TextBlock {
                parent_document_id: content_ref.content_id,
                ..TextBlock::default()
            }.key()
        ];
        let text_blocks = DynaDB::batchget_table(CONTENT_TABLE.into(), text_block_keys).await;

        if text_blocks.is_some() {
            let mut doc: DocStream = DynaDB::get(
                CONTENT_TABLE.into(),
                doc_key
            ).await?;
            doc = DocStream {
                media: media.await,
                text_blocks,
                ..doc
            };
            Some(doc)
        } else {
            None
        }
    }

    pub async fn create(doc: DocStream) -> Option<DocStreamRef> {
        let existing_doc: Option<DocStream> = DynaDB::get(CONTENT_TABLE.into(), doc.clone().key()).await;
        match existing_doc {
            Some(_) => None,
            None => DocStream::persist(doc).await
        }
    }

    pub async fn update(doc: DocStream) -> Option<DocStreamRef> {
        let existing_doc: DocStream = DynaDB::get(CONTENT_TABLE.into(), doc.clone().key()).await?;
        let content_ref = DocStreamRef {
            namespace: existing_doc.namespace,
            content_id: existing_doc.content_id,
            ..DocStreamRef::default()
        };
        DocStream::delete(content_ref).await?;
        DocStream::persist(doc).await
    }

    pub async fn delete(content_ref: DocStreamRef) -> Option<DocStreamRef> {
        let doc_key = DocStream {
            namespace: content_ref.clone().namespace,
            content_id: content_ref.clone().content_id,
            ..DocStream::default()
        }.key();
        let existing_doc: Option<DocStream> = DynaDB::get(CONTENT_TABLE.into(), doc_key.clone()).await;

        match existing_doc {
            Some(doc) => {
                let mut delete_handles = vec![];
                delete_handles.push(doc_key);
                match doc.clone().media {
                    None => (),
                    Some(media) => {
                        media.into_iter().for_each(|m| {
                            delete_handles.push(m.key())
                        });
                    }
                };
                match doc.clone().text_blocks {
                    None => (),
                    Some(text_block) => {
                        text_block.into_iter().for_each(|m| {
                            delete_handles.push(m.key())
                        });
                    }
                };
                DynaDB::batchdelete_table(CONTENT_TABLE.into(), delete_handles).await?;
                Some(content_ref)
            },
            None => None
        }
    }

    async fn persist(doc: DocStream) -> Option<DocStreamRef> {
        let mut full_doc_data = vec![];
        let doc_data = DocStream {
            media: None, // Ignore media list in this payload and handle it after in batch
            text_blocks: None, // Ignore text_blocks list in this payload and handle it after in batch
            ..doc.clone()
        }.drain();
        full_doc_data.push(doc_data);
        match doc.clone().media {
            None => (),
            Some(media) => {
                media.into_iter().for_each(|m| {
                    full_doc_data.push(m.drain())
                });
            }
        };
        match doc.clone().text_blocks {
            None => (),
            Some(text_block) => {
                text_block.into_iter().for_each(|m| {
                    full_doc_data.push(m.drain())
                });
            }
        };
        let tables_data: Vec<HashMap<String, Vec<HashMap<String, AttributeValue>>>> = vec![
            hash_map! {
                CONTENT_TABLE.into() => full_doc_data
            }
        ];

        let content_ref = Some(
            DocStreamRef {
                namespace: doc.clone().namespace,
                content_id: doc.clone().content_id
            }
        );

        match DynaDB::batchput_data(tables_data.clone()).await {
            Some(output) => match output.unprocessed_items {
                Some(unprocessed_items) => {
                    if unprocessed_items.is_empty() {
                        content_ref
                    } else {
                        error!("CREATE_DOCUMENT_ERROR:::: Unprocessed Items: {:?}", unprocessed_items);
                        DynaDB::batchdelete_data(tables_data).await;
                        None
                    }
                }
                None => content_ref
            }
            None => {
                error!("CREATE_DOCUMENT_ERROR:::: Could write Data: {:?}", tables_data);
                None
            }
        }
    }
}

impl ModelDynaConv for DocStream {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                NAMESPACE => self.namespace = value.s,
                CONTENT_ID => self.content_id = value.s,
                USER_ID => self.user_id = value.s,
                SCORE => self.score = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                VERSION => self.version = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                CREATED_AT => self.created_at= value.s,
                UPDATED_AT => self.updated_at = value.s,
                TAGS => self.tags = value.ss,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (NAMESPACE.into(), attr_s(self.namespace)),
                (CONTENT_ID.into(), attr_s(self.content_id)),
                (USER_ID.into(), attr_s(self.user_id)),
                (SCORE.into(), attr_n(self.score)),
                (VERSION.into(), attr_n(self.version)),
                (CREATED_AT.into(), attr_s(self.created_at)),
                (UPDATED_AT.into(), attr_s(self.updated_at)),
                (TAGS.into(), attr_ss(self.tags)),
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (NAMESPACE.into(), attr_s(self.namespace)),
                (CONTENT_ID.into(), attr_s(self.content_id))
            ]
                .iter().cloned().collect();
        key
    }
}

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct DocStreamRef {
    pub namespace: Option<String>,
    pub content_id: Option<String>,
}

impl ModelDynaConv for DocStreamRef {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                NAMESPACE => self.namespace = value.s,
                CONTENT_ID => self.content_id = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        unimplemented!()
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        unimplemented!()
    }
}
