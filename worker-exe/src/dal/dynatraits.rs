
use rusoto_dynamodb::{
    AttributeValue, BatchGetItemInput, DeleteItemInput, GetItemInput, KeysAndAttributes, PutItemInput, QueryInput
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
            return_consumed_capacity: None,
        }
    }

    fn with_keys(&mut self, table_name: String, keys: Vec<HashMap<String, AttributeValue>>) -> Self {
        let kt = KeysAndAttributes{
            attributes_to_get: None,
            consistent_read: None,
            expression_attribute_names: None,
            keys,
            projection_expression: None,
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
            attributes_to_get: None,
            consistent_read: None,
            expression_attribute_names: None,
            key: HashMap::new(),
            projection_expression: None,
            return_consumed_capacity: None,
            table_name: table,
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
            condition_expression: None,
            conditional_operator: None,
            expected: None,
            expression_attribute_names: None,
            expression_attribute_values: None,
            item: HashMap::new(),
            return_consumed_capacity: None,
            return_item_collection_metrics: None,
            return_values: None,
            table_name: table,
        }
    }

    fn with_item(&mut self, item: HashMap<String, AttributeValue>) -> Self {
        self.item = item;
        self.clone()
    }
}

pub trait QueryInputBuilder {
    fn new(table: String) -> Self;
    fn with_data(&mut self, data: HashMap<String, AttributeValue>) -> Self;
    fn with_key_condition_expr(&mut self, expr: String) -> Self;
}

impl QueryInputBuilder for QueryInput {
    fn new(table: String) -> Self {
        QueryInput {
            attributes_to_get: None,
            conditional_operator: None,
            consistent_read: None,
            exclusive_start_key: None,
            expression_attribute_names: None,
            expression_attribute_values: None,
            filter_expression: None,
            index_name: None,
            key_condition_expression: None,
            key_conditions: None,
            limit: None,
            projection_expression: None,
            query_filter: None,
            return_consumed_capacity: None,
            scan_index_forward: None,
            select: None,
            table_name: table,
        }
    }

    fn with_data(&mut self, data: HashMap<String, AttributeValue>) -> Self {
        self.expression_attribute_values = Some(data);
        self.clone()
    }

    fn with_key_condition_expr(&mut self, expr: String) -> Self {
        self.key_condition_expression = Some(expr);
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
            condition_expression: None,
            conditional_operator: None,
            expected: None,
            expression_attribute_names: None,
            expression_attribute_values: None,
            key: HashMap::new(),
            return_consumed_capacity: None,
            return_item_collection_metrics: None,
            return_values: None,
            table_name: table,
        }
    }

    fn with_key(&mut self, key: HashMap<String, AttributeValue>) -> Self {
        self.key = key;
        self.clone()
    }
}

pub trait AttributeValueBuilder {
    fn new() -> Self;
    fn with_b(&mut self, b: Option<Vec<u8>>) -> Self;
    fn with_bool(&mut self, bool: Option<bool>) -> Self;
    fn with_bs(&mut self, bs: Option<Vec<Vec<u8>>>) -> Self;
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
            b: None,
            bool: None,
            bs: None,
            l: None,
            m: None,
            n: None,
            ns: None,
            null: None,
            s: None,
            ss: None,
        }
    }

    fn with_b(&mut self, b: Option<Vec<u8>>) -> Self {
        self.b = b;
        self.clone()
    }

    fn with_bool(&mut self, bool: Option<bool>) -> Self {
        self.bool = bool;
        self.clone()
    }

    fn with_bs(&mut self, bs: Option<Vec<Vec<u8>>>) -> Self {
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
