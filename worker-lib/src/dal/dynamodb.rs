
use rusoto_core::Region;
use rusoto_dynamodb::{
    DynamoDb, // This is Required!!!!
    AttributeValue, BatchGetItemInput, DynamoDbClient, ListTablesInput,
    DeleteItemInput, GetItemInput, PutItemInput, QueryInput
};
use std::collections::HashMap;
use dal::{
    AttributeValueBuilder, BatchGetItemInputBuilder, DeleteItemInputBuilder,
    GetItemInputBuilder, QueryInputBuilder, PutItemInputBuilder, ModelDynaConv
};

// Group all DynamoDB API calls to this Struct
pub struct DynaDB {}

impl DynaDB {

    pub fn list_tables() -> Option<Vec<String>> {

        let client = DynamoDbClient::new(Region::UsEast1);
        let list_table_input: ListTablesInput = Default::default();

        match client.list_tables(list_table_input).sync() {
            Ok(output) => output.table_names,
            Err(error) => {
                error!("LIST_TABLE_ERROR:::: {}", error);
                None
            }
        }
    }

    // CRUD API

    pub fn put(table: String, data: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {
        let response = _put(table, data);
        response
    }

    pub fn get<T: ModelDynaConv>(table: String, key: HashMap<String, AttributeValue>) -> Option<T> {
        let response = _get(table, key);
        match response {
            Some(data) => Some(T::new()
                .hydrate(data.clone())),
            None => None
        }
    }

    // BatchGets Items Of A Single Table
    pub fn batchget_table<T: ModelDynaConv>(table: String, keys: Vec<HashMap<String, AttributeValue>>) -> Option<Vec<T>> {
        debug!("Batch Getting: {} => keys {:?}", table, keys);
        let response = _batchget(table.clone(), keys);
        match response {
            Some(res) => match res.get(&table) {
                Some(vec_t) => Some(vec_t.iter().map(|data|
                    T::new()
                        .hydrate(data.clone()))
                    .collect()),
                None => None
            },
            None => None
        }
    }

    pub fn query<T: ModelDynaConv>(table: String, data: Option<HashMap<String, AttributeValue>>, filter_expr: Option<String>, key_condition_expr: Option<String>) -> Option<Vec<T>> {
        let response = _query(table, data, filter_expr, key_condition_expr);

        match response {
            Some(res) => Some(res.iter().map(|data|
                T::new()
                    .hydrate(data.clone()))
                .collect()),
            None => None
        }
    }

    pub fn delete(table: String, data: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {
        let response = _delete(table, data);
        response
    }
}

// CRUD Helpers

fn _put(table: String, item: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {

    let put_input = PutItemInput::new(table)
        .with_item(item);

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.put_item(put_input).sync() {
        Ok(output) => output.attributes,
        Err(error) => {
            error!("TABLE_PUTITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

fn _get(table: String, key: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {

    let get_input = GetItemInput::new(table)
        .with_key(key);

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.get_item(get_input).sync() {
        Ok(output) => output.item,
        Err(error) => {
            error!("TABLE_GETITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

fn _batchget(table: String, keys: Vec<HashMap<String, AttributeValue>>) -> Option<HashMap<String, Vec<::std::collections::HashMap<String, AttributeValue>>>> {

    let batchget_input = BatchGetItemInput::new()
        .with_keys(table, keys);

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.batch_get_item(batchget_input).sync() {
        Ok(output) => output.responses,
        Err(error) => {
            error!("TABLE_BATCHGETITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

fn _query(table: String, data: Option<HashMap<String, AttributeValue>>, filter_expr: Option<String>, key_condition_expr: Option<String>) -> Option<Vec<HashMap<String, AttributeValue>>> {

    let query_input = QueryInput::new(table)
        .with_data(data)
        .with_filter_expr(filter_expr)
        .with_key_condition_expr(key_condition_expr);

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.query(query_input).sync() {
        Ok(output) => output.items,
        Err(error) => {
            error!("TABLE_QUERYITEMS_ERROR:::: {}", error);
            None
        }
    };

    results
}

fn _delete(table: String, key: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {

    let delete_input = DeleteItemInput::new(table)
        .with_key(key);

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.delete_item(delete_input).sync() {
        Ok(output) => output.attributes,
        Err(error) => {
            error!("TABLE_DELETEITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

// Attribute Helpers

pub fn attr_b(b: Option<Vec<u8>>) -> AttributeValue {
    let attr = AttributeValue::new().with_b(b);
    attr
}

pub fn attr_bool(bool: Option<bool>) -> AttributeValue {
    let attr = AttributeValue::new().with_bool(bool);
    attr
}

pub fn attr_bs(bs: Option<Vec<Vec<u8>>>) -> AttributeValue {
    let attr = AttributeValue::new().with_bs(bs);
    attr
}

pub fn attr_l(l: Option<Vec<AttributeValue>>) -> AttributeValue {
    let attr = AttributeValue::new().with_l(l);
    attr
}

pub fn attr_m(m: Option<HashMap<String, AttributeValue>>) -> AttributeValue {
    let attr = AttributeValue::new().with_m(m);
    attr
}

pub fn attr_n(n: Option<i32>) -> AttributeValue {
    let attr = AttributeValue::new().with_n(n.and_then(|i| Some(i.to_string())));
    attr
}

pub fn attr_ns(ns: Option<Vec<i32>>) -> AttributeValue {
    let attr = AttributeValue::new().with_ns(ns.and_then(|lst_i|
        Some(
            lst_i.iter().map(|i| i.to_string()).collect()
        )));
    attr
}

pub fn attr_null(null: Option<bool>) -> AttributeValue {
    let attr = AttributeValue::new().with_null(null);
    attr
}

pub fn attr_s(s: Option<String>) -> AttributeValue {
    let attr = AttributeValue::new().with_s(s);
    attr
}

pub fn attr_ss(ss: Option<Vec<String>>) -> AttributeValue {
    let attr = AttributeValue::new().with_ss(ss);
    attr
}
