use std::collections::HashMap;

use rusoto_dynamodb::AttributeValue;

use crate::dal::dynamodb::{attr_n, attr_s, attr_ss, attr_bool, DynaDB};

use crate::dal::dynatraits::ModelDynaConv;
use crate::dal::models::constants::*;
use std::collections::hash_map::RandomState;

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct TextBlock {
    pub parent_document_id: Option<String>,
    pub text_block_id: Option<String>,
    pub user_id: Option<String>,
    pub tags: Option<Vec<String>>,
    pub score: Option<i32>,
    pub version: Option<i32>,
    pub position: Option<i32>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
    pub text_block_type : Option<String>,
    pub text: Option<String>,
    pub author: Option<String>,
}

impl ModelDynaConv for TextBlock {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                PARENT_DOCUMENT_ID => self.parent_document_id = value.s,
                TEXT_BLOCK_ID => self.text_block_id = value.s,
                USER_ID => self.user_id = value.s,
                TAGS => self.tags = value.ss,
                SCORE => self.score = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                VERSION => self.version = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                POSITION => self.position = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                CREATED_AT => self.created_at= value.s,
                UPDATED_AT => self.updated_at = value.s,
                TYPE => self.text_block_type = value.s,
                TEXT => self.text = value.s,
                AUTHOR => self.author = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (PARENT_DOCUMENT_ID.into(), attr_s(self.parent_document_id)),
                (TEXT_BLOCK_ID.into(), attr_s(self.text_block_id)),
                (USER_ID.into(), attr_s(self.user_id)),
                (TAGS.into(), attr_ss(self.tags)),
                (SCORE.into(), attr_n(self.score)),
                (VERSION.into(), attr_n(self.version)),
                (POSITION.into(), attr_n(self.position)),
                (CREATED_AT.into(), attr_s(self.created_at)),
                (UPDATED_AT.into(), attr_s(self.updated_at)),
                (TYPE.into(), attr_s(self.text_block_type)),
                (TEXT.into(), attr_s(self.text)),
                (AUTHOR.into(), attr_s(self.author)),
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (NAMESPACE.into(), attr_s(self.parent_document_id)),
                (CONTENT_ID.into(), attr_s(self.text_block_id))
            ]
                .iter().cloned().collect();
        key
    }
}
