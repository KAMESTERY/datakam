
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
                (String::from(TOPIC), attr_s(self.topic)),
                (String::from(DOCUMENT_ID), attr_s(self.document_id)),
                (String::from(USER_ID), attr_s(self.user_id)),
                (String::from(TAGS), attr_ss(self.tags)),
                (String::from(SCORE), attr_n(self.score)),
                (String::from(VERSION), attr_n(self.version)),
                (String::from(CREATED_AT), attr_s(self.created_at)),
                (String::from(UPDATED_AT), attr_s(self.updated_at)),
                (String::from(SLUG), attr_s(self.slug)),
                (String::from(TITLE), attr_s(self.title)),
                (String::from(IDENTIFIER), attr_s(self.identifier)),
                (String::from(BODY), attr_s(self.body)),
                (String::from(PUBLISH), attr_bool(self.publish)),
                (String::from(FILTRE_VISUEL), attr_n(self.filtre_visuel)),
                (String::from(LANGUE), attr_n(self.langue)),
                (String::from(NIVEAU), attr_n(self.niveau)),
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (String::from(TOPIC), attr_s(self.topic)),
                (String::from(DOCUMENT_ID), attr_s(self.document_id))
            ]
                .iter().cloned().collect();
        key
    }
}

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct Media {}
