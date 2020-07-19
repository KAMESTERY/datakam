
use std::collections::HashMap;

use rusoto_dynamodb::AttributeValue;

use crate::dal::dynamodb::{attr_n, attr_s, attr_ss, attr_bool, DynaDB};
use crate::dal::dynatraits::ModelDynaConv;

const TOPIC: &str = "Topic";
const DOCUMENT_ID: &str = "DocumentID";
const USER_ID: &str = "UserID";
const TAGS: &str = "Tags";
const SCORE: &str = "Score";
const VERSION: &str = "Version";
const CREATED_AT: &str = "CreatedAt";
const UPDATED_AT: &str = "UpdatedAt";
const SLUG: &str = "Slug";
const TITLE: &str = "Title";
const IDENTIFIER: &str = "Identifier";
const BODY: &str = "Body";
const PUBLISH: &str = "Publish";
const FILTRE_VISUEL: &str = "FiltreVisuel";
const LANGUE: &str = "Langue";
const NIVEAU: &str = "Niveau";

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
                (TOPIC.into(), attr_s(self.topic)),
                (DOCUMENT_ID.into(), attr_s(self.document_id))
            ]
                .iter().cloned().collect();
        key
    }
}

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct Media {}
