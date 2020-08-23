
use std::collections::HashMap;

use rusoto_dynamodb::AttributeValue;

use crate::dal::dynamodb::{attr_n, attr_s, attr_ss, attr_bool, DynaDB};

use crate::dal::dynatraits::ModelDynaConv;
use crate::dal::models::constants::*;

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct Media {
    pub parent_document_id: Option<String>,
    pub media_type: Option<String>,
    pub media_id: Option<String>,
    pub user_id: Option<String>,
    pub tags: Option<Vec<String>>,
    pub score: Option<i32>,
    pub version: Option<i32>,
    pub position: Option<i32>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
    pub file_url: Option<String>
}

impl ModelDynaConv for Media {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                PARENT_DOCUMENT_ID => self.parent_document_id = value.s,
                TYPE => self.media_type = value.s,
                MEDIA_ID => self.media_id = value.s,
                USER_ID => self.user_id = value.s,
                TAGS => self.tags = value.ss,
                SCORE => self.score = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                VERSION => self.version = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                POSITION => self.position = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                CREATED_AT => self.created_at= value.s,
                UPDATED_AT => self.updated_at = value.s,
                FILE_URL => self.updated_at = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (PARENT_DOCUMENT_ID.into(), attr_s(self.parent_document_id)),
                (TYPE.into(), attr_s(self.media_type)),
                (MEDIA_ID.into(), attr_s(self.media_id)),
                (USER_ID.into(), attr_s(self.user_id)),
                (TAGS.into(), attr_ss(self.tags)),
                (SCORE.into(), attr_n(self.score)),
                (VERSION.into(), attr_n(self.version)),
                (POSITION.into(), attr_n(self.position)),
                (CREATED_AT.into(), attr_s(self.created_at)),
                (UPDATED_AT.into(), attr_s(self.updated_at)),
                (FILE_URL.into(), attr_s(self.file_url)),
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let namespace = match self.parent_document_id {
            Some(p_id) => Some(p_id),
            None => self.media_type
        };
        let key: HashMap<String, AttributeValue> =
            [
                (NAMESPACE.into(), attr_s(namespace)),
                (CONTENT_ID.into(), attr_s(self.media_id))
            ]
                .iter().cloned().collect();
        key
    }
}
