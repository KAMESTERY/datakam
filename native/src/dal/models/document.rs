
use std::collections::HashMap;

use rusoto_dynamodb::AttributeValue;

use crate::dal::dynamodb::{attr_n, attr_s, attr_ss, attr_bool, DynaDB};
use crate::dal::dynatraits::ModelDynaConv;
use crate::dal::models::constants::*;
use crate::dal::models::content::{ContentRef};
use crate::dal::models::media::{Media};

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct Document {
    pub topic: Option<String>,
    pub document_id: Option<String>,
    pub user_id: Option<String>,
    pub tags: Option<Vec<String>>,
    pub score: Option<i32>,
    pub version: Option<i32>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
    pub slug : Option<String>,
    pub title: Option<String>,
    pub identifier: Option<String>,
    pub body : Option<String>,
    pub publish: Option<bool>,
    pub filtre_visuel: Option<i32>,
    pub langue: Option<i32>,
    pub niveau: Option<i32>,
    pub  media: Option<Vec<Media>>
}

impl Document {
    // API Functions
    pub async fn get(content_ref: ContentRef) -> Option<Document> {
        let doc_key = Document {
            topic: content_ref.clone().namespace,
            document_id: content_ref.clone().content_id,
            ..Document::default()
        }.key();
        let media_keys = vec![
            Media {
                parent_document_id: content_ref.content_id,
                ..Media::default()
            }.key()
        ];

        let media = DynaDB::batchget_table(CONTENT_TABLE.into(), media_keys);
        let mut doc: Document = DynaDB::get(CONTENT_TABLE.into(), doc_key).await?;
        doc = Document {
            media: media.await,
            ..doc
        };

        Some(doc)
    }

    pub async fn create(doc: Document) -> Option<ContentRef> {
        let existing_doc: Option<Document> = DynaDB::get(CONTENT_TABLE.into(), doc.clone().key()).await;
        match existing_doc {
            Some(_) => None,
            None => Document::persist(doc).await
        }
    }

    // pub async fn create(doc: Document) -> Option<ContentRef> {
    //     let existing_doc: Option<Document> = DynaDB::get(CONTENT_TABLE.into(), doc.key()).await;
    //     match existing_doc {
    //         Some(_) => None,
    //         None => {
    //             let put_response = DynaDB::put(CONTENT_TABLE.into(), doc.drain()).await?;
    //             let content_ref = ContentRef::default().hydrate(put_response);
    //             Some(content_ref)
    //         }
    //     }
    // }

    pub async fn update(doc: Document) -> Option<ContentRef> {
        let existing_doc: Document = DynaDB::get(CONTENT_TABLE.into(), doc.clone().key()).await?;
        let content_ref = ContentRef {
            namespace: existing_doc.topic,
            content_id: existing_doc.document_id,
            ..ContentRef::default()
        };
        Document::delete(content_ref).await?;
        Document::persist(doc).await
    }

    pub async fn delete(content_ref: ContentRef) -> Option<ContentRef> {
        let doc_key = Document {
            topic: content_ref.clone().namespace,
            document_id: content_ref.clone().content_id,
            ..Document::default()
        }.key();
        let existing_doc: Document = DynaDB::get(CONTENT_TABLE.into(), doc_key.clone()).await?;

        let mut delete_handles = vec![];
        delete_handles.push(doc_key);
        match existing_doc.clone().media {
            None => (),
            Some(media) => {
                media.into_iter().for_each(|m| {
                    delete_handles.push(m.key())
                });
            }
        };
        DynaDB::batchdelete_table(CONTENT_TABLE.into(), delete_handles).await?;
        Some(content_ref)
    }

    async fn persist(doc: Document) -> Option<ContentRef> {
        let mut full_doc_data = vec![];
        let doc_data = Document {
            media: None, // Ignore media list in this payload and handle it after in batch
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
        let tables_data: Vec<HashMap<String, Vec<HashMap<String, AttributeValue>>>> = vec![
            hash_map! {
                CONTENT_TABLE.into() => full_doc_data
            }
        ];

        let content_ref = Some(
            ContentRef {
                namespace: doc.clone().topic,
                content_id: doc.clone().document_id
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

impl ModelDynaConv for Document {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                TOPIC => self.topic = value.s,
                DOCUMENT_ID => self.document_id = value.s,
                USER_ID => self.user_id = value.s,
                TAGS => self.tags = value.ss,
                SCORE => self.score = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                VERSION => self.version = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                CREATED_AT => self.created_at= value.s,
                UPDATED_AT => self.updated_at = value.s,
                SLUG => self.slug = value.s,
                TITLE => self.slug = value.s,
                IDENTIFIER => self.identifier = value.s,
                BODY => self.body = value.s,
                PUBLISH => self.publish = value.bool,
                FILTRE_VISUEL => self.filtre_visuel = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                LANGUE => self.langue = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                NIVEAU => self.niveau = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (TOPIC.into(), attr_s(self.topic)),
                (DOCUMENT_ID.into(), attr_s(self.document_id)),
                (USER_ID.into(), attr_s(self.user_id)),
                (TAGS.into(), attr_ss(self.tags)),
                (SCORE.into(), attr_n(self.score)),
                (VERSION.into(), attr_n(self.version)),
                (CREATED_AT.into(), attr_s(self.created_at)),
                (UPDATED_AT.into(), attr_s(self.updated_at)),
                (SLUG.into(), attr_s(self.slug)),
                (TITLE.into(), attr_s(self.title)),
                (IDENTIFIER.into(), attr_s(self.identifier)),
                (BODY.into(), attr_s(self.body)),
                (PUBLISH.into(), attr_bool(self.publish)),
                (FILTRE_VISUEL.into(), attr_n(self.filtre_visuel)),
                (LANGUE.into(), attr_n(self.langue)),
                (NIVEAU.into(), attr_n(self.niveau)),
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (NAMESPACE.into(), attr_s(self.topic)),
                (CONTENT_ID.into(), attr_s(self.document_id))
            ]
                .iter().cloned().collect();
        key
    }
}
