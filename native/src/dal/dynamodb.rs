use bytes::Bytes;
use rusoto_core::region::Region;
use rusoto_dynamodb::{
    DynamoDb, // This is Required!!!!
    AttributeValue, DynamoDbClient,
    ListTablesInput, DeleteItemInput,
    GetItemInput, PutItemInput,
    QueryInput, ScanInput,
    BatchGetItemInput, BatchWriteItemInput, BatchWriteItemOutput
};
use std::collections::HashMap;
use crate::dal::{
    ModelDynaConv,
    AttributeValueBuilder, DeleteItemInputBuilder, GetItemInputBuilder,
    QueryInputBuilder, ScanInputBuilder, PutItemInputBuilder,
    BatchGetItemInputBuilder, BatchWriteItemInputBuilder
};

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct ScanParams {
    table: String,
    index_name: Option<String>,
    expr_attr_names: Option<HashMap<String, String>>,
    data: Option<HashMap<String, AttributeValue>>,
    filter_expr: Option<String>,
    projection_expr: Option<String>,
    limit: Option<i64>
}

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct QueryParams {
    table: String,
    index_name: Option<String>,
    expr_attr_names: Option<HashMap<String, String>>,
    data: Option<HashMap<String, AttributeValue>>,
    filter_expr: Option<String>,
    key_condition_expr: Option<String>,
    projection_expr: Option<String>,
    select: Option<String>,
    limit: Option<i64>
}

// Group all DynamoDB API calls to this Struct
pub struct DynaDB {}

impl DynaDB {

    pub async fn list_tables() -> Option<Vec<String>> {

        let client = DynamoDbClient::new(Region::UsEast1);
        let list_table_input: ListTablesInput = Default::default();

        match client.list_tables(list_table_input).await {
            Ok(output) => output.table_names,
            Err(error) => {
                error!("LIST_TABLE_ERROR:::: {}", error);
                None
            }
        }
    }

    // CRUD API

    pub async fn put(table: String, data: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {
        let response = _put(table, data).await;
        response
    }

    pub async fn get<T: ModelDynaConv>(table: String, key: HashMap<String, AttributeValue>) -> Option<T> {
        let response = _get(table, key).await;
        match response {
            Some(data) => Some(T::default()
                .hydrate(data.clone())),
            None => None
        }
    }

    // BatchGets Items Of A Single Table
    pub async fn batchget_table<T: ModelDynaConv>(table: String, keys: Vec<HashMap<String, AttributeValue>>) -> Option<Vec<T>> {
        debug!("Batch Getting: {} => keys {:?}", table, keys);
        let response = _batchget(table.clone(), keys).await;
        match response {
            Some(res) => match res.get(&table) {
                Some(vec_t) => Some(vec_t.iter().map(|data|
                    T::default()
                        .hydrate(data.clone()))
                    .collect()),
                None => None
            },
            None => None
        }
    }

    // BatchPuts Items To Multiple Tables
    pub async fn batchput_data(tables_data: Vec<HashMap<String, Vec<HashMap<String, AttributeValue>>>>) -> Option<BatchWriteItemOutput> {

        let mut batchwrite_input = BatchWriteItemInput::new();

        tables_data.into_iter().for_each(|tbl_data| {
            for (table, data) in tbl_data {
                batchwrite_input.with_put_items(table, data);
            }
        });

        let batchwrite_output = _batchwrite(batchwrite_input).await;

        batchwrite_output
    }

    // BatchPuts Items To A Single Table
    pub async fn batchput_table(table: String, data: Vec<HashMap<String, AttributeValue>>) -> Option<BatchWriteItemOutput> {

        let batchwrite_input = BatchWriteItemInput::new()
            .with_put_items(table, data);

        let batchwrite_output = _batchwrite(batchwrite_input).await;

        batchwrite_output
    }

    // BatchDeletes Items From A Single Table
    pub async fn batchdelete_table(table: String, data: Vec<HashMap<String, AttributeValue>>) -> Option<BatchWriteItemOutput> {

        let batchwrite_input = BatchWriteItemInput::new()
            .with_delete_items(table, data);

        let batchwrite_output = _batchwrite(batchwrite_input).await;

        batchwrite_output
    }

    // BatchDeletes Items From Multiple Tables
    pub async fn batchdelete_data(tables_data: Vec<HashMap<String, Vec<HashMap<String, AttributeValue>>>>) -> Option<BatchWriteItemOutput> {

        let mut batchwrite_input = BatchWriteItemInput::new();

        tables_data.into_iter().for_each(|tbl_data| {
            for (table, data) in tbl_data {
                batchwrite_input.with_delete_items(table, data);
            }
        });

        let batchwrite_output = _batchwrite(batchwrite_input).await;

        batchwrite_output
    }

    pub async fn query<T: ModelDynaConv>(
        queryParams: QueryParams
    ) -> Option<Vec<T>> {

        let response = _query(
            queryParams.table,
            queryParams.index_name,
            queryParams.expr_attr_names,
            queryParams.data,
            queryParams.filter_expr,
            queryParams.key_condition_expr,
            queryParams.projection_expr,
            queryParams.select,
            queryParams.limit
        ).await;

        match response {
            Some(res) => Some(res.iter().map(|data|
                T::default()
                    .hydrate(data.clone()))
                .collect()),
            None => None
        }
    }

    #[deprecated(note = "Please use function accepting input struct QueryParams")]
    pub async fn query_old<T: ModelDynaConv>(
        table: String,
        index_name: Option<String>,
        expr_attr_names: Option<HashMap<String, String>>,
        data: Option<HashMap<String, AttributeValue>>,
        filter_expr: Option<String>,
        key_condition_expr: Option<String>,
        projection_expr: Option<String>,
        select: Option<String>,
        limit: Option<i64>
    ) -> Option<Vec<T>> {
        
        let response = _query(
            table,
            index_name,
            expr_attr_names,
            data,
            filter_expr,
            key_condition_expr,
            projection_expr,
            select,
            limit
        ).await;

        match response {
            Some(res) => Some(res.iter().map(|data|
                T::default()
                    .hydrate(data.clone()))
                .collect()),
            None => None
        }
    }

    pub async fn scan<T: ModelDynaConv>(
        scanParams: ScanParams
    ) -> Option<Vec<T>> {

        let response = _scan(
            scanParams.table,
            scanParams.index_name,
            scanParams.expr_attr_names,
            scanParams.data,
            scanParams.filter_expr,
            scanParams.projection_expr,
            scanParams.limit
        ).await;

        match response {
            Some(res) => Some(res.iter().map(|data|
                T::default()
                    .hydrate(data.clone()))
                .collect()),
            None => None
        }
    }

    #[deprecated(note = "Please use function accepting input struct ScanParams")]
    pub async fn scan_old<T: ModelDynaConv>(
        table: String,
        index_name: Option<String>,
        expr_attr_names: Option<HashMap<String, String>>,
        data: Option<HashMap<String, AttributeValue>>,
        filter_expr: Option<String>,
        projection_expr: Option<String>,
        limit: Option<i64>
    ) -> Option<Vec<T>> {
        
        let response = _scan(
            table,
            index_name,
            expr_attr_names,
            data,
            filter_expr,
            projection_expr,
            limit
        ).await;
        
        match response {
            Some(res) => Some(res.iter().map(|data|
                T::default()
                    .hydrate(data.clone()))
                .collect()),
            None => None
        }
    }

    pub async fn delete(table: String, data: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {
        let response = _delete(table, data).await;
        response
    }
}

// CRUD Helpers

async fn _put(table: String, item: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {

    let put_input = PutItemInput::new(table)
        .with_item(item);

    dbg!(put_input.clone());

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.put_item(put_input).await {
        Ok(output) => output.attributes,
        Err(error) => {
            error!("TABLE_PUTITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

async fn _get(table: String, key: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {

    let get_input = GetItemInput::new(table)
        .with_key(key);

    dbg!(get_input.clone());

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.get_item(get_input).await {
        Ok(output) => output.item,
        Err(error) => {
            error!("TABLE_GETITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

async fn _batchget(table: String, keys: Vec<HashMap<String, AttributeValue>>) -> Option<HashMap<String, Vec<::std::collections::HashMap<String, AttributeValue>>>> {

    let batchget_input = BatchGetItemInput::new()
        .with_keys(table, keys);

    dbg!(batchget_input.clone());

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.batch_get_item(batchget_input).await {
        Ok(output) => output.responses,
        Err(error) => {
            error!("TABLE_BATCHGETITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

async fn _batchwrite(batchwrite_input: BatchWriteItemInput) -> Option<BatchWriteItemOutput> {

    dbg!(batchwrite_input.clone());

    let client = DynamoDbClient::new(Region::UsEast1);

    match client.batch_write_item(batchwrite_input).await {
        Ok(batchwrite_output) => Some(batchwrite_output),
        Err(error) => {
            error!("TABLE_BATCHWRITE_ERROR:::: {}", error);
            None
        }
    }
}

async fn _query(
    table: String,
    index_name: Option<String>,
    expr_attr_names: Option<HashMap<String, String>>,
    data: Option<HashMap<String, AttributeValue>>,
    filter_expr: Option<String>,
    key_condition_expr: Option<String>,
    projection_expr: Option<String>,
    select: Option<String>,
    limit: Option<i64>
) -> Option<Vec<HashMap<String, AttributeValue>>> {

    let query_input = QueryInput::new(table)
        .with_index_name(index_name)
        .with_expr_attr_names(expr_attr_names)
        .with_data(data)
        .with_filter_expr(filter_expr)
        .with_key_condition_expr(key_condition_expr)
        .with_projection_expression(projection_expr)
        .with_select(select)
        .with_limit(limit);

    dbg!(query_input.clone());

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.query(query_input).await {
        Ok(output) => output.items,
        Err(error) => {
            error!("TABLE_QUERYITEMS_ERROR:::: {}", error);
            None
        }
    };

    results
}

async fn _scan(
    table: String,
    index_name: Option<String>,
    expr_attr_names: Option<HashMap<String, String>>,
    data: Option<HashMap<String, AttributeValue>>,
    filter_expr: Option<String>,
    projection_expr: Option<String>,
    limit: Option<i64>
) -> Option<Vec<HashMap<String, AttributeValue>>> {
    
    let scan_input = ScanInput::new(table)
        .with_index_name(index_name)
        .with_expr_attr_names(expr_attr_names)
        .with_data(data)
        .with_filter_expr(filter_expr)
        .with_projection_expression(projection_expr)
        .with_limit(limit);

    dbg!(scan_input.clone());

    let client = DynamoDbClient::new(Region::UsEast1);
    
    let results = match client.scan(scan_input).await {
        Ok(output) => output.items,
        Err(error) => {
            error!("TABLE_SCAN_ERROR:::: {}", error);
            None
        }
    };
    
    results
}

async fn _delete(table: String, key: HashMap<String, AttributeValue>) -> Option<HashMap<String, AttributeValue>> {

    let delete_input = DeleteItemInput::new(table)
        .with_key(key);

    dbg!(delete_input.clone());

    let client = DynamoDbClient::new(Region::UsEast1);

    let results = match client.delete_item(delete_input).await {
        Ok(output) => output.attributes,
        Err(error) => {
            error!("TABLE_DELETEITEM_ERROR:::: {}", error);
            None
        }
    };

    results
}

// Attribute Helpers

pub fn attr_b(b: Option<Bytes>) -> AttributeValue {
    let attr = AttributeValue::new().with_b(b);
    attr
}

pub fn attr_bool(bool: Option<bool>) -> AttributeValue {
    let attr = AttributeValue::new().with_bool(bool);
    attr
}

pub fn attr_bs(bs: Option<Vec<Bytes>>) -> AttributeValue {
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
