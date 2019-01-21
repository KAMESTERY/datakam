
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
//    debug!("Transformed VecOfVec {:?} into Map {:?}", data, hm);
    hm
}

#[derive(GraphQLInputObject)]
#[graphql(description = "A QueryInput :-)")]
pub struct QueryInput {
    pub index_name: Option<String>,
    pub attr_names: Option<Vec<Vec<String>>>,
    pub filter_expr: Option<String>,
    pub key_condition_expr: Option<String>,
    pub projection_expr: Option<String>,
    pub select: Option<String>,
    pub limit: Option<i32>,
    pub raw_data: Option<Vec<Vec<String>>>
}

#[derive(GraphQLInputObject)]
#[graphql(description = "A DocumentInput :-)")]
pub struct DocumentInput {
    pub name: String,
    pub user_id: String,
    pub document_id: String,
    pub data: Vec<Vec<String>>,
    pub tags: Option<Vec<String>>
}

impl ThingDataTrait for DocumentInput {
    fn get_data(&self) -> Vec<Vec<String>> {
        self.data.clone()
    }
}

#[derive(GraphQLInputObject)]
#[graphql(description = "A MediaInput :-)")]
pub struct MediaInput {
    pub name: String,
    pub user_id: String,
    pub media_id: String,
    pub data: Vec<Vec<String>>,
    pub tags: Option<Vec<String>>
}

impl ThingDataTrait for MediaInput {
    fn get_data(&self) -> Vec<Vec<String>> {
        self.data.clone()
    }
}

pub fn create_media_items(media: Vec<MediaInput>) -> Vec<String> {
    media.par_iter().map(|m| {
        match Thing::get_thing(m.name.clone(), m.media_id.clone()) {
            Some(_thing) =>{
                let err_msg = String::from("");
                warn!("Media Already Exists! {} ==> {}", m.name.clone(), m.media_id.clone());
                err_msg
            },
            None => {
                match m.data_as_hashmap().ok() {
                    Some(data_map) => create_thing(
                        m.name.clone(),
                        m.user_id.clone(),
                        data_map,
                        m.media_id.clone(),
                        m.tags.clone()),
                    None => String::from("")
                }
            }
        }
    }).collect()
}

pub fn update_media_items(media: Vec<MediaInput>) -> Vec<String> {
    media.par_iter().for_each(|doc| {
        delete_complete_thing(doc.name.clone(), doc.media_id.clone());
    });
    create_media_items(media)
}

pub fn delete_media_items(data: Vec<Vec<String>>) -> Vec<String> {
    data.par_iter().map(|vec_kv| {
        let (name, thing_id) = (vec_kv[0].clone(), vec_kv[1].clone());
        delete_complete_thing(name, thing_id).unwrap_or_default()
    }).collect()
}

pub fn create_documents(documents: Vec<DocumentInput>) -> Vec<String> {
    documents.par_iter().map(|doc| {
        match Thing::get_thing(doc.name.clone(), doc.document_id.clone()) {
            Some(_thing) =>{
                let err_msg = String::from("");
                warn!("Document Already Exists! {} ==> {}", doc.name.clone(), doc.document_id.clone());
                err_msg
            },
            None => {
                match doc.data_as_hashmap().ok() {
                    Some(data_map) => create_thing(
                        doc.name.clone(),
                        doc.user_id.clone(),
                        data_map,
                        doc.document_id.clone(),
                        doc.tags.clone()),
                    None => String::from("")
                }
            }
        }
    }).collect()
}

pub fn update_documents(documents: Vec<DocumentInput>) -> Vec<String> {
    documents.par_iter().for_each(|doc| {
        delete_complete_thing(doc.name.clone(), doc.document_id.clone());
    });
    create_documents(documents)
}

pub fn delete_documents(data: Vec<Vec<String>>) -> Vec<String> {
    data.par_iter().map(|vec_kv| {
        let (name, thing_id) = (vec_kv[0].clone(), vec_kv[1].clone());
        delete_complete_thing(name, thing_id).unwrap_or_default()
    }).collect()
}

#[derive(GraphQLInputObject)]
#[graphql(description = "A ThingInput :-)")]
pub struct ThingInput {
    pub name: String,
    pub user_id: String,
    pub data: Vec<Vec<String>>,
    pub tags: Option<Vec<String>>
}

impl ThingDataTrait for ThingInput {
    fn get_data(&self) -> Vec<Vec<String>> {
        self.data.clone()
    }
}

pub fn create_complete_thing(name: String, user_id: String, data: HashMap<String, String>, tags: Option<Vec<String>>) -> String {
    let thing_id = format!("{}", Uuid::new_v4());
    create_thing(name, user_id, data, thing_id, tags)
}

#[derive(Clone, Debug, GraphQLObject)]
pub struct ThingOutput {
    pub thing: Thing,
    pub data: Vec<Data>
}

impl ThingOutput {
    pub fn scan_les_choses(
        _user_id: String,
        index_name: Option<String>,
        raw_attr_names: Option<Vec<Vec<String>>>,
        filter_expr: Option<String>,
        projection_expr: Option<String>,
        limit: Option<i64>,
        raw_data: Option<Vec<Vec<String>>>) -> Option<Vec<ThingOutput>> {

        let expr_attr_names = match raw_attr_names {
            Some(attr_names) => Some(data_as_hashmap(attr_names)),
            None => None
        };

        let mut data= HashMap::new();
        if raw_data.is_some() {
            let d = data_as_hashmap(raw_data.unwrap());
            for (k, v) in d {
                data.insert(k, attr_s(Some(v)));
            }
        }

        let things: Vec<Thing> = DynaDB::scan(
            String::from("Things"),
            index_name,
            expr_attr_names,
            Some(data),
            filter_expr,
            projection_expr,
            limit
        )?;

        ThingOutput::to_thingouput_vec(things)
    }

    pub fn query_les_choses(
        index_name: Option<String>,
        attr_names: Option<Vec<Vec<String>>>,
        filter_expr: Option<String>,
        key_condition_expr: Option<String>,
        projection_expr: Option<String>,
        select: Option<String>,
        limit: Option<i64>,
        raw_data: Option<Vec<Vec<String>>>) -> Option<Vec<ThingOutput>> {

        let expr_attr_names = match attr_names {
            Some(attr_names) => Some(data_as_hashmap(attr_names)),
            None => None
        };

        let mut data= HashMap::new();
        if raw_data.is_some() {
            let d = data_as_hashmap(raw_data.unwrap());
            for (k, v) in d {
                data.insert(k, attr_s(Some(v)));
            }
        }

        let things: Vec<Thing> = DynaDB::query(
            String::from("Things"),
            index_name,
            expr_attr_names,
            Some(data),
            filter_expr,
            key_condition_expr,
            projection_expr,
            select,
            limit
        )?;

        ThingOutput::to_thingouput_vec(things)
    }

    pub fn get_les_choses(data: Vec<Vec<String>>) -> Option<Vec<ThingOutput>> {

        let thing_keys = data.iter().map(|vec_kv| {
            let (name, thing_id) = (vec_kv[0].clone(), vec_kv[1].clone());
            [
                (String::from("ThingID"), attr_s(Some(thing_id))),
                (String::from("Name"), attr_s(Some(name)))
            ].iter().cloned().collect()
        }).collect();
        let things: Vec<Thing> = DynaDB::batchget_table(String::from("Things"), thing_keys)?;
        ThingOutput::to_thingouput_vec(things)
    }

    fn get_thing_data(thing_id: Option<String>) -> Option<Vec<Data>> {
        let expr_attr_names: HashMap<String, String> = [
            (String::from("#tid"), String::from("ThingID"))
        ].iter().cloned().collect();
        let qdata: HashMap<String, AttributeValue> = [
            (String::from(":tid"), attr_s(thing_id))
        ].iter().cloned().collect();
        let data: Option<Vec<Data>> = DynaDB::query(
            String::from("Data"),
            None,
            Some(expr_attr_names),
            Some(qdata),
            None,
            Some(String::from("#tid = :tid")),
            None,
            None,
            None
        );
        data
    }

    pub fn get_thing_output(name: String, user_id: String) -> Option<ThingOutput> {
        let thing = Thing::get_thing(name, user_id)?;
        let data = ThingOutput::get_thing_data(thing.clone().thing_id)?;
        Some(
            ThingOutput { thing, data }
        )
    }

    fn to_thingouput_vec(things: Vec<Thing>) -> Option<Vec<ThingOutput>> {
        let list_of_choses = things.clone().par_iter().map(|thing: &Thing| {
            let data = ThingOutput::get_thing_data(thing.clone().thing_id)?;
            Some(ThingOutput { thing: thing.to_owned(), data })
        }).filter(|c| {
            c.is_some()
        }).map(|x| x.unwrap()).collect();
        Some(list_of_choses)
    }
}

fn create_thing(name: String, user_id: String, data: HashMap<String, String>, thing_id: String, tags: Option<Vec<String>>) -> String {

    let mut tables_data: Vec<HashMap<String, Vec<HashMap<String, AttributeValue>>>> = Vec::new();

    let now_string = Utc::now().to_string(); // ISO 8601
    let created_at = String::from(now_string.clone());
    let updated_at = String::from(now_string);
    let version = 0;
    let score = 0;

    let thing_tbl = String::from("Things");
    let mut thing_tbl_data = HashMap::new();
    let thing_values = Thing::new()
        .with_name(name)
        .with_user_id(user_id)
        .with_thing_id(thing_id.clone())
        .with_version(version)
        .with_score(score)
        .with_created_at(created_at)
        .with_updated_at(updated_at)
        .with_tags(tags)
        .drain();
    thing_tbl_data.insert(thing_tbl, vec![thing_values]);
    tables_data.push(thing_tbl_data);

    let data_tbl = String::from("Data");
    let mut data_tbl_data = HashMap::new();
    let mut data_tbl_data_vec = Vec::new();
    for (key, value) in data {
        let data_id = format!("{}", Uuid::new_v4());
        let datum_values = Data::new()
            .with_data_id(data_id)
            .with_thing_id(thing_id.clone())
            .with_key(key)
            .with_value(value)
            .drain();
        data_tbl_data_vec.push(datum_values);
    }
    data_tbl_data.insert(data_tbl, data_tbl_data_vec);
    tables_data.push(data_tbl_data);

//    debug!("BatchPut Tables Data: {:?}", tables_data.clone());

    match DynaDB::batchput_data(tables_data.clone()) {
        Some(output) => match output.unprocessed_items {
            Some(unprocessed_items) => {
                if unprocessed_items.is_empty() {
                    thing_id
                } else {
                    error!("CREATE_THING_ERROR:::: Unprocessed Items: {:?}", unprocessed_items);
                    DynaDB::batchdelete_data(tables_data);
                    String::from("")
                }
            }
            None => thing_id
        }
        None => {
            error!("CREATE_THING_ERROR:::: Could write Data: {:?}", tables_data);
            String::from("")
        }
    }
}

pub fn delete_complete_thing(name: String, thing_id: String) -> Option<String> {

    let mut tables_data: Vec<HashMap<String, Vec<HashMap<String, AttributeValue>>>> = Vec::new();

    let mut thing_tbl_data = HashMap::new();
    let thing_key = Thing::new()
        .with_name(name)
        .with_thing_id(thing_id.clone())
        .key();
    thing_tbl_data.insert(String::from("Things"), vec![thing_key]);
    tables_data.push(thing_tbl_data);

    let mut data_tbl_data = HashMap::new();
    let data = ThingOutput::get_thing_data(Some(thing_id.clone()))?;
    let delete_handles: Vec<HashMap<String, AttributeValue>> = data.iter()
        .map(|datum: &Data| datum.clone().key()).collect();
    data_tbl_data.insert(String::from("Data"), delete_handles.clone());
    tables_data.push(data_tbl_data);

    match DynaDB::batchdelete_data(tables_data.clone()) {
        Some(output) => match output.unprocessed_items {
            Some(unprocessed_items) => {
                if unprocessed_items.is_empty() {
                    Some(thing_id)
                } else {
                    error!("DELETE_THING_ERROR:::: Unprocessed Items: {:?}", unprocessed_items);
                    Some(String::from(""))
                }
            }
            None => Some(thing_id)
        }
        None => {
            error!("DELETE_THING_ERROR:::: Could write Data: {:?}", tables_data);
            Some(String::from(""))
        }
    }
}

#[derive(Default, Clone, Debug, GraphQLObject)]
pub struct Thing {
    pub name: Option<String>,
    pub user_id: Option<String>,
    pub thing_id: Option<String>,
    pub version: Option<i32>,
    pub score: Option<i32>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
    pub tags: Option<Vec<String>>
}

impl Thing {
    // API Functions
    pub fn create_thing(
        name: String, user_id: String,
        thing_id: String, version: i32, score: i32,
        created_at: String, updated_at: String,
        tags: Option<Vec<String>>
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
            .with_tags(tags)
            .drain();

//        debug!("Put Thing Values: {:?}", thing_values);

        let put_response = DynaDB::put(String::from("Things"), thing_values);

        debug!("Put Response: {:?}", put_response);

        put_response
    }

    pub fn get_thing(name: String, thing_id: String) -> Option<Thing> {
        let key = Thing::new()
            .with_name(name)
            .with_thing_id(thing_id)
            .key();

//        debug!("Get User Key: {:?}", key);

        let thing: Option<Thing> = DynaDB::get(String::from("Things"), key.clone());

//        debug!("Thing: {:?}", thing);

        thing
    }

    pub fn delete_thing(name: String, thing_id: String) -> Option<HashMap<String, AttributeValue>> {
        let key = Thing::new()
            .with_name(name)
            .with_thing_id(thing_id)
            .key();

//        debug!("Delete Thing Key: {:?}", key);

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
    pub fn with_tags(&mut self, tags: Option<Vec<String>>) -> Self {
        self.tags = tags;
        self.clone()
    }
}

impl ModelDynaConv for Thing {
    fn new() -> Self {
        Thing::default()
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
                "Tags" => self.tags = value.ss,
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
                (String::from("Tags"), attr_ss(self.tags))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (String::from("Name"), attr_s(Some(self.name.unwrap_or(String::from("NoName"))))),
                (String::from("ThingID"), attr_s(Some(self.thing_id.unwrap_or(String::from("NoThingID")))))
            ]
                .iter().cloned().collect();
        key
    }
}

#[derive(Default, Clone, Debug, GraphQLObject)]
pub struct Data {
    pub data_id: Option<String>,
    pub thing_id: Option<String>,
    pub key: Option<String>,
    pub value: Option<String>
}

impl Data {
    pub fn new() -> Self {
        Data::default()
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

//        debug!("Put Data Values: {:?}", datum_values);

        let put_response = DynaDB::put(String::from("Data"), datum_values);

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub fn get_datum(thing_id: String, data_id: String) -> Option<Data> {
        let key = Data::new()
            .with_thing_id(thing_id)
            .with_data_id(data_id)
            .key();

//        debug!("Get Data Key: {:?}", key.clone());

        let datum: Option<Data> = DynaDB::get(String::from("Data"), key);

        debug!("Data: {:?}", datum);

        datum
    }
    pub fn delete_data(thing_id: String) -> Option<Vec<HashMap<String, AttributeValue>>> {

        let data = ThingOutput::get_thing_data(Some(thing_id))?;

        let delete_handles: Vec<HashMap<String, AttributeValue>> = data.iter()
            .map(|datum: &Data| datum.clone().key()).collect();

        DynaDB::batchdelete_table(String::from("Data"), delete_handles.clone());

        debug!("Deleted Data Handles: {:?}", delete_handles.clone());

        Some(delete_handles)
    }
    pub fn delete_datum(thing_id: String, data_id: String) -> Option<HashMap<String, AttributeValue>> {
        let key = Data::new()
            .with_thing_id(thing_id)
            .with_data_id(data_id)
            .key();

//        debug!("Delete Data Key: {:?}", key);

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
        Data::default()
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
