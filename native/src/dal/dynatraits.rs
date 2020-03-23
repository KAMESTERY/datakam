use bytes::Bytes;
use rusoto_dynamodb::{
    AttributeValue, BatchGetItemInput,
    DeleteItemInput, GetItemInput,
    KeysAndAttributes, PutItemInput,
    QueryInput, ScanInput,
    BatchWriteItemInput, WriteRequest,
    DeleteRequest, PutRequest
};
use std::collections::HashMap;

pub trait ModelDynaConv {
    fn new() -> Self;
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self;
    fn drain(self) -> HashMap<String, AttributeValue>;
    fn key(self) -> HashMap<String, AttributeValue>;
}

pub trait BatchGetItemInputBuilder {
    fn new() -> Self;
    fn with_keys(&mut self, table_name: String, keys: Vec<HashMap<String, AttributeValue>>) -> Self;
}

impl BatchGetItemInputBuilder for BatchGetItemInput {
    fn new() -> Self {
        BatchGetItemInput {
            request_items: HashMap::new(),
            ..BatchGetItemInput::default()
        }
    }

    fn with_keys(&mut self, table_name: String, keys: Vec<HashMap<String, AttributeValue>>) -> Self {
        let kt = KeysAndAttributes{
            keys,
            ..KeysAndAttributes::default()
        };
        self.request_items.insert(table_name, kt);
        self.clone()
    }
}

pub trait GetItemInputBuilder {
    fn new(table: String) -> Self;
    fn with_key(&mut self, key: HashMap<String, AttributeValue>) -> Self;
}

impl GetItemInputBuilder for GetItemInput {
    fn new(table: String) -> Self {
        GetItemInput {
            table_name: table,
            key: HashMap::new(),
            ..GetItemInput::default()
        }
    }

    fn with_key(&mut self, key: HashMap<String, AttributeValue>) -> Self {
        self.key = key;
        self.clone()
    }
}

pub trait PutItemInputBuilder {
    fn new(table: String) -> Self;
    fn with_item(&mut self, item: HashMap<String, AttributeValue>) -> Self;
}

impl PutItemInputBuilder for PutItemInput {
    fn new(table: String) -> Self {
        PutItemInput {
            table_name: table,
            item: HashMap::new(),
            ..PutItemInput::default()
        }
    }

    fn with_item(&mut self, item: HashMap<String, AttributeValue>) -> Self {
        self.item = item;
        self.clone()
    }
}

pub trait BatchWriteItemInputBuilder {
    fn new() -> Self;
    fn with_put_items(&mut self, table: String, data: Vec<HashMap<String, AttributeValue>>) -> Self;
    fn with_delete_items(&mut self, table: String, data: Vec<HashMap<String, AttributeValue>>) -> Self;
}

impl BatchWriteItemInputBuilder for BatchWriteItemInput {
    fn new() -> Self {
        BatchWriteItemInput::default()
    }

    fn with_put_items(&mut self, table: String, data: Vec<HashMap<String, AttributeValue>>) -> Self {

        let items_data = data.iter().map(|datum| {
            WriteRequest {
                put_request: Some(PutRequest { item: datum.clone() }),
                delete_request: None
            }
        }).collect();

        self.request_items.insert(table, items_data);

        self.clone()
    }

    fn with_delete_items(&mut self, table: String, data: Vec<HashMap<String, AttributeValue>>) -> Self {

        let items_data = data.iter().map(|datum| {
            WriteRequest {
                put_request: None,
                delete_request: Some(DeleteRequest { key: datum.clone() })
            }
        }).collect();

        self.request_items.insert(table, items_data);

        self.clone()
    }
}

pub trait QueryInputBuilder {
    fn new(table: String) -> Self;
    fn with_index_name(&mut self, idx_name: Option<String>) -> Self;
    fn with_expr_attr_names(&mut self, expr_attr_names: Option<HashMap<String, String>>) -> Self;
    fn with_data(&mut self, data: Option<HashMap<String, AttributeValue>>) -> Self;
    fn with_key_condition_expr(&mut self, expr: Option<String>) -> Self;
    fn with_filter_expr(&mut self, expr: Option<String>) -> Self;
    fn with_projection_expression(&mut self, expr: Option<String>) -> Self;
    fn with_select(&mut self, select: Option<String>) -> Self;
    fn with_limit(&mut self, limit: Option<i64>) -> Self;
}

impl QueryInputBuilder for QueryInput {
    fn new(table: String) -> Self {
        QueryInput {
            table_name: table,
            ..QueryInput::default()
        }
    }
    
    fn with_index_name(&mut self, idx_name: Option<String>) -> Self {
        self.index_name = idx_name;
        self.clone()
    }
    
    fn with_expr_attr_names(&mut self, expr_attr_names: Option<HashMap<String, String>>) -> Self {
        self.expression_attribute_names = expr_attr_names;
        self.clone()
    }
    
    fn with_data(&mut self, data: Option<HashMap<String, AttributeValue>>) -> Self {
        self.expression_attribute_values = data;
        self.clone()
    }

    fn with_key_condition_expr(&mut self, expr: Option<String>) -> Self {
        self.key_condition_expression = expr;
        self.clone()
    }

    fn with_filter_expr(&mut self, expr: Option<String>) -> Self {
        self.filter_expression = expr;
        self.clone()
    }
    
    fn with_projection_expression(&mut self, expr: Option<String>) -> Self {
        self.projection_expression = expr;
        self.clone()
    }

    fn with_select(&mut self, select: Option<String>) -> Self {
        self.select = select;
        self.clone()
    }

    fn with_limit(&mut self, limit: Option<i64>) -> Self {
        self.limit = limit;
        self.clone()
    }
}

pub trait ScanInputBuilder {
    fn new(table: String) -> Self;
    fn with_index_name(&mut self, idx_name: Option<String>) -> Self;
    fn with_expr_attr_names(&mut self, expr_attr_names: Option<HashMap<String, String>>) -> Self;
    fn with_data(&mut self, data: Option<HashMap<String, AttributeValue>>) -> Self;
    fn with_filter_expr(&mut self, expr: Option<String>) -> Self;
    fn with_projection_expression(&mut self, expr: Option<String>) -> Self;
    fn with_limit(&mut self, limit: Option<i64>) -> Self;
}

impl ScanInputBuilder for ScanInput {
    fn new(table: String) -> Self {
        ScanInput {
            table_name: table,
            ..ScanInput::default()
        }
    }
    
    fn with_index_name(&mut self, idx_name: Option<String>) -> Self {
        self.index_name = idx_name;
        self.clone()
    }
    
    fn with_expr_attr_names(&mut self, expr_attr_names: Option<HashMap<String, String>>) -> Self {
        self.expression_attribute_names = expr_attr_names;
        self.clone()
    }
    
    fn with_data(&mut self, data: Option<HashMap<String, AttributeValue>>) -> Self {
        self.expression_attribute_values = data;
        self.clone()
    }
    
    fn with_filter_expr(&mut self, expr: Option<String>) -> Self {
        self.filter_expression = expr;
        self.clone()
    }
    
    fn with_projection_expression(&mut self, expr: Option<String>) -> Self {
        self.projection_expression = expr;
        self.clone()
    }

    fn with_limit(&mut self, limit: Option<i64>) -> Self {
        self.limit = limit;
        self.clone()
    }
}

pub trait DeleteItemInputBuilder {
    fn new(table: String) -> Self;
    fn with_key(&mut self, key: HashMap<String, AttributeValue>) -> Self;
}

impl DeleteItemInputBuilder for DeleteItemInput {
    fn new(table: String) -> Self {
        DeleteItemInput {
            table_name: table,
            key: HashMap::new(),
            ..DeleteItemInput::default()
        }
    }

    fn with_key(&mut self, key: HashMap<String, AttributeValue>) -> Self {
        self.key = key;
        self.clone()
    }
}

pub trait AttributeValueBuilder {
    fn new() -> Self;
    fn with_b(&mut self, b: Option<Bytes>) -> Self;
    fn with_bool(&mut self, bool: Option<bool>) -> Self;
    fn with_bs(&mut self, bs: Option<Vec<Bytes>>) -> Self;
    fn with_l(&mut self, l: Option<Vec<AttributeValue>>) -> Self;
    fn with_m(&mut self, m: Option<::std::collections::HashMap<String, AttributeValue>>) -> Self;
    fn with_n(&mut self, n: Option<String>) -> Self;
    fn with_ns(&mut self, ns: Option<Vec<String>>) -> Self;
    fn with_null(&mut self, null: Option<bool>) -> Self;
    fn with_s(&mut self, s: Option<String>) -> Self;
    fn with_ss(&mut self, ss: Option<Vec<String>>) -> Self;
}

impl AttributeValueBuilder for AttributeValue {
    fn new() -> Self {
        AttributeValue {
            ..AttributeValue::default()
        }
    }

    fn with_b(&mut self, b: Option<Bytes>) -> Self {
        self.b = b;
        self.clone()
    }

    fn with_bool(&mut self, bool: Option<bool>) -> Self {
        self.bool = bool;
        self.clone()
    }

    fn with_bs(&mut self, bs: Option<Vec<Bytes>>) -> Self {
        self.bs = bs;
        self.clone()
    }

    fn with_l(&mut self, l: Option<Vec<AttributeValue>>) -> Self {
        self.l = l;
        self.clone()
    }

    fn with_m(&mut self, m: Option<HashMap<String, AttributeValue>>) -> Self {
        self.m = m;
        self.clone()
    }

    fn with_n(&mut self, n: Option<String>) -> Self {
        self.n = n;
        self.clone()
    }

    fn with_ns(&mut self, ns: Option<Vec<String>>) -> Self {
        self.ns = ns;
        self.clone()
    }

    fn with_null(&mut self, null: Option<bool>) -> Self {
        self.null = null;
        self.clone()
    }

    fn with_s(&mut self, s: Option<String>) -> Self {
        self.s = s;
        self.clone()
    }

    fn with_ss(&mut self, ss: Option<Vec<String>>) -> Self {
        self.ss = ss;
        self.clone()
    }
}
