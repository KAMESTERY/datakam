
use std::collections::HashMap;

use rusoto_dynamodb::AttributeValue;

use crate::dal::ModelDynaConv;
use crate::dal::models::constants::*;

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct ContentRef {
    pub namespace: Option<String>,
    pub content_id: Option<String>,
}

impl ModelDynaConv for ContentRef {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                TOPIC => self.namespace = value.s,
                DOCUMENT_ID => self.content_id = value.s,
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
