
use rusoto_dynamodb::{AttributeValue};
use std::collections::HashMap;
use uuid::Uuid;
use rayon::prelude::*;
use chrono::prelude::*;
use crate::dal::dynatraits::{ModelDynaConv};
use crate::dal::dynamodb::{attr_n, attr_s, attr_ss, DynaDB};

pub trait ThingDataTrait {
    fn get_data(&self) -> Vec<Vec<String>>;
    fn data_as_hashmap(&self) -> Result<HashMap<String, String>, &'static str> {
        Ok(data_as_hashmap(self.get_data().clone()))
    }
}

pub fn data_as_hashmap(data: Vec<Vec<String>>) -> HashMap<String, String> {
    let mut hm = HashMap::new();
    data.clone().into_iter().for_each(|v| {
        let k: String = v.get(0)
            .expect("No Key Provided")
            .to_string();
        let v: String = v.get(1)
            .expect("No Value Provided")
            .to_string();
        hm.insert(k, v);
    });
    debug!("Transformed VecOfVec {:?} into Map {:?}", data, hm);
    hm
}

#[derive(GraphQLInputObject)]
#[graphql(description = "A DocumentInput :-)")]
pub struct DocumentInput {
    pub name: String,
    pub user_id: String,
    pub document_id: String,
    pub data: Vec<Vec<String>>
}

impl ThingDataTrait for DocumentInput {
    fn get_data(&self) -> Vec<Vec<String>> {
        self.data.clone()
    }
}

pub fn create_documents(documents: Vec<DocumentInput>) -> Vec<String> {
    documents.par_iter().map(|doc| {
        match doc.data_as_hashmap().ok() {
            Some(data_map) => create_thing(doc.name.clone(), doc.user_id.clone(), data_map, doc.document_id.clone()),
            None => String::from("")
        }
    }).collect()
}

#[derive(GraphQLInputObject)]
#[graphql(description = "A ThingInput :-)")]
pub struct ThingInput {
    pub name: String,
    pub user_id: String,
    pub data: Vec<Vec<String>>
//    pub data: Vec<(String, String)>
}

impl ThingDataTrait for ThingInput {
    fn get_data(&self) -> Vec<Vec<String>> {
        self.data.clone()
    }
}

pub fn create_complete_thing(name: String, user_id: String, data: HashMap<String, String>) -> String {
    let thing_id = format!("{}", Uuid::new_v4());
    create_thing(name, user_id, data, thing_id)
}

#[derive(Clone, Debug, GraphQLObject)]
pub struct ThingOutput {
    pub thing: Thing,
    pub data: Vec<Data>
}

impl ThingOutput {
    pub fn query_les_choses(_user_id: String, filter_expr: Option<String>, key_condition_expr: Option<String>, raw_data: Option<Vec<Vec<String>>>) -> Option<Vec<ThingOutput>> {
        let mut data= HashMap::new();
        if raw_data.is_some() {
            let d = data_as_hashmap(raw_data.unwrap());
            for (k, v) in d {
                data.insert(k, attr_s(Some(v)));
            }
        }
        let things: Vec<Thing> = DynaDB::query(String::from("Things"), Some(data), filter_expr, key_condition_expr)?;
        ThingOutput::to_thingouput_vec(things)
    }

    pub fn get_les_choses(user_id: String, names: Vec<String>) -> Option<Vec<ThingOutput>> {
        let thing_keys = names.into_iter().map(|name| {
            [
                (String::from("UserID"), attr_s(Some(user_id.clone()))),
                (String::from("Name"), attr_s(Some(name)))
            ].iter().cloned().collect()
        }).collect();
        let things: Vec<Thing> = DynaDB::batchget_table(String::from("Things"), thing_keys)?;
        ThingOutput::to_thingouput_vec(things)
    }

    pub fn get_thing_output(name: String, user_id: String) -> Option<ThingOutput> {
        let thing = Thing::get_thing(name, user_id)?;
        let data_ids = thing.clone().data_ids?;
        let keys = data_ids.into_iter().map(|id| {
            [
                (String::from("ThingID"), attr_s(thing.clone().thing_id)),
                (String::from("DataID"), attr_s(Some(id)))
            ].iter().cloned().collect()
        }).collect();
        let data: Vec<Data> = DynaDB::batchget_table(String::from("Data"), keys)?;
        Some(
            ThingOutput { thing, data }
        )
    }

    fn to_thingouput_vec(things: Vec<Thing>) -> Option<Vec<ThingOutput>> {
        let list_of_choses = things.clone().iter().map(|thing: &Thing| {
            let data_ids = thing.to_owned().data_ids?;
            let data_keys = data_ids.into_iter().map(|id| {
                [
                    (String::from("ThingID"), attr_s(thing.to_owned().thing_id)),
                    (String::from("DataID"), attr_s(Some(id)))
                ].iter().cloned().collect()
            }).collect();
            let data: Vec<Data> = DynaDB::batchget_table(String::from("Data"), data_keys)?;
            Some(ThingOutput { thing: thing.to_owned(), data })
        }).filter(|c| {
            c.is_some()
        }).map(|x| x.unwrap()).collect();
        Some(list_of_choses)
    }
}

fn create_thing(name: String, user_id: String, data: HashMap<String, String>, thing_id: String) -> String {
    let mut data_ids = Vec::new();
    for (k, v) in data {
        let data_id = Uuid::new_v4();
        let data_id_string = format!("{}", data_id);
        Data::create_datum(
            data_id_string.clone(),
            thing_id.clone(),
            k,
            v
        );
        data_ids.push(data_id_string)
    }
    let now_string = Utc::now().to_string(); // ISO 8601
    let created_at = String::from(now_string.clone());
    let updated_at = String::from(now_string);
    let version = 0;
    let score = 0;
    Thing::create_thing(
        name.clone(),
        user_id,
        thing_id,
        version,
        score,
        created_at,
        updated_at,
        data_ids
    );
    name
}

pub fn delete_complete_thing(name: String, user_id: String) -> Option<String> {
    let thing = Thing::get_thing(
        name.clone(),
        user_id.clone()
    )?;
    let thing_id = thing.thing_id?;
    let data_ids = thing.data_ids?;
    data_ids.iter().for_each(|data_id: &String| {
        let _ = Data::delete_datum(
            thing_id.clone(),
            data_id.to_owned()
        );
    });
    Thing::delete_thing(name, user_id);
    Some(String::from("DELETED"))
}

#[derive(Clone, Debug, GraphQLObject)]
pub struct Thing {
    pub name: Option<String>,
    pub user_id: Option<String>,
    pub thing_id: Option<String>,
    pub version: Option<i32>,
    pub score: Option<i32>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
    pub data_ids: Option<Vec<String>>
}

impl Thing {
    pub fn new() -> Self {
        Thing {
            name: None,
            user_id: None,
            thing_id: None,
            version: None,
            score: None,
            created_at: None,
            updated_at: None,
            data_ids: None,
        }
    }
    // API Functions
    pub fn create_thing(
        name: String, user_id: String,
        thing_id: String, version: i32, score: i32,
        created_at: String, updated_at: String, data_ids: Vec<String>
    ) -> Option<HashMap<String, AttributeValue>> {
        // TODO: Create Data for this Thing
        let thing_values = Thing::new()
            .with_name(name)
            .with_user_id(user_id)
            .with_thing_id(thing_id)
            .with_version(version)
            .with_score(score)
            .with_created_at(created_at)
            .with_updated_at(updated_at)
            .with_data_ids(data_ids)
            .drain();

        debug!("Put Thing Values: {:?}", thing_values);

        let put_response = DynaDB::put(String::from("Things"), thing_values);

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub fn get_thing(name: String, user_id: String) -> Option<Thing> {
        let key = Thing::new()
            .with_name(name)
            .with_user_id(user_id)
            .key();

        debug!("Get User Key: {:?}", key);

        let thing: Option<Thing> = DynaDB::get(String::from("Things"), key.clone());

        debug!("Thing: {:?}", thing);

        thing
    }

    pub fn delete_thing(name: String, user_id: String) -> Option<HashMap<String, AttributeValue>> {
        let key = Thing::new()
            .with_name(name)
            .with_user_id(user_id)
            .key();

        debug!("Delete Thing Key: {:?}", key);

        let result = DynaDB::delete(String::from("Things"), key);
        debug!("Result: {:?}", result.clone());

        result
    }
    // Builder Function
    pub fn with_name(&mut self, name:String) -> Self {
        self.name = Some(name);
        self.clone()
    }
    pub fn with_user_id(&mut self, user_id: String) -> Self {
        self.user_id = Some(user_id);
        self.clone()
    }
    pub fn with_thing_id(&mut self, thing_id:String) -> Self {
        self.thing_id = Some(thing_id);
        self.clone()
    }
    pub fn with_version(&mut self, version:i32) -> Self {
        self.version = Some(version);
        self.clone()
    }
    pub fn with_score(&mut self, score:i32) -> Self {
        self.score = Some(score);
        self.clone()
    }
    pub fn with_created_at(&mut self, created_at:String) -> Self {
        self.created_at = Some(created_at);
        self.clone()
    }
    pub fn with_updated_at(&mut self, updated_at:String) -> Self {
        self.updated_at = Some(updated_at);
        self.clone()
    }
    pub fn with_data_ids(&mut self, data_ids:Vec<String>) -> Self {
        self.data_ids = Some(data_ids);
        self.clone()
    }
}

impl ModelDynaConv for Thing {
    fn new() -> Self {
        Thing {
            name: None,
            user_id: None,
            thing_id: None,
            version: None,
            score: None,
            created_at: None,
            updated_at: None,
            data_ids: None,
        }
    }

    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                "Name" => self.name = value.s,
                "UserID" => self.user_id = value.s,
                "ThingID" => self.thing_id = value.s,
                "Version" => self.version = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                "Score" => self.score = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                "CreatedAt" => self.created_at= value.s,
                "UpdatedAt" => self.updated_at = value.s,
                "DataIDs" => self.data_ids = value.ss,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (String::from("Name"), attr_s(self.name)),
                (String::from("UserID"), attr_s(self.user_id)),
                (String::from("ThingID"), attr_s(self.thing_id)),
                (String::from("Version"), attr_n(self.version)),
                (String::from("Score"), attr_n(self.score)),
                (String::from("CreatedAt"), attr_s(self.created_at)),
                (String::from("UpdatedAt"), attr_s(self.updated_at)),
                (String::from("DataIDs"), attr_ss(self.data_ids))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (String::from("Name"), attr_s(Some(self.name.unwrap_or(String::from("NoName"))))),
                (String::from("UserID"), attr_s(Some(self.user_id.unwrap_or(String::from("NoUserID")))))
            ]
                .iter().cloned().collect();
        key
    }
}

#[derive(Clone, Debug, GraphQLObject)]
pub struct Data {
    pub data_id: Option<String>,
    pub thing_id: Option<String>,
    pub key: Option<String>,
    pub value: Option<String>
}

impl Data {
    pub fn new() -> Self {
        Data{
            data_id: None,
            thing_id: None,
            key: None,
            value: None,
        }
    }
    // API Functions
    pub fn create_datum(
        data_id: String, thing_id: String,
        key: String, value: String
    ) -> Option<HashMap<String, AttributeValue>> {
        // TODO: Create Data for this Thing
        let datum_values = Data::new()
            .with_data_id(data_id)
            .with_thing_id(thing_id)
            .with_key(key)
            .with_value(value)
            .drain();

        debug!("Put Data Values: {:?}", datum_values);

        let put_response = DynaDB::put(String::from("Data"), datum_values);

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub fn get_datum(thing_id: String, data_id: String) -> Option<Data> {
        let key = Data::new()
            .with_thing_id(thing_id)
            .with_data_id(data_id)
            .key();

        debug!("Get Data Key: {:?}", key.clone());

        let datum: Option<Data> = DynaDB::get(String::from("Data"), key);

        debug!("Data: {:?}", datum);

        datum
    }
    pub fn delete_datum(thing_id: String, data_id: String) -> Option<HashMap<String, AttributeValue>> {
        let key = Data::new()
            .with_thing_id(thing_id)
            .with_data_id(data_id)
            .key();

        debug!("Delete Data Key: {:?}", key);

        let result = DynaDB::delete(String::from("Data"), key);
        debug!("Result: {:?}", result.clone());

        result
    }
    // Builder Function
    pub fn with_data_id(&mut self, data_id: String) -> Self {
        self.data_id = Some(data_id);
        self.clone()
    }
    pub fn with_thing_id(&mut self, thing_id: String) -> Self {
        self.thing_id = Some(thing_id);
        self.clone()
    }
    pub fn with_key(&mut self, key: String) -> Self {
        self.key = Some(key);
        self.clone()
    }
    pub fn with_value(&mut self, value: String) -> Self {
        self.value = Some(value);
        self.clone()
    }
}

impl ModelDynaConv for Data {
    fn new() -> Self {
        Data{
            data_id: None,
            thing_id: None,
            key: None,
            value: None,
        }
    }

    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                "DataID" => self.data_id = value.s,
                "ThingID" => self.thing_id = value.s,
                "Key" => self.key = value.s,
                "Value" => self.value = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (String::from("DataID"), attr_s(self.data_id)),
                (String::from("ThingID"), attr_s(self.thing_id)),
                (String::from("Key"), attr_s(self.key)),
                (String::from("Value"), attr_s(self.value))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (String::from("DataID"), attr_s(Some(self.data_id.unwrap_or(String::from("NoDataID"))))),
                (String::from("ThingID"), attr_s(Some(self.thing_id.unwrap_or(String::from("NoThingID")))))
            ]
                .iter().cloned().collect();
        key
    }
}
