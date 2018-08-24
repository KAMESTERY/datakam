
use std::cell::Cell;
use std::sync::Arc;

//use actix::prelude::*;
use actix::prelude::{Actor, Handler, Message, SyncContext};
use actix::Addr;
use actix_web::{Error};

use serde_json;

use juniper::http::GraphQLRequest;

use schema::Schema;

// This struct represents the counter state
pub struct AppState {
    pub executor: Addr<GraphQLExecutor>,
    pub count: Cell<usize>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphQLData(GraphQLRequest);

impl Message for GraphQLData {
    type Result = Result<String, Error>;
}

pub struct GraphQLExecutor {
    schema: Arc<Schema>,
}

impl GraphQLExecutor {
    pub fn new(schema: Arc<Schema>) -> GraphQLExecutor {
        GraphQLExecutor { schema: schema }
    }
}

impl Actor for GraphQLExecutor {
    type Context = SyncContext<Self>;
}

impl Handler<GraphQLData> for GraphQLExecutor {
    type Result = Result<String, Error>;

    fn handle(&mut self, msg: GraphQLData, _: &mut Self::Context) -> Self::Result {
        let res = msg.0.execute(&self.schema, &());
        let res_text = serde_json::to_string(&res)?;
        Ok(res_text)
    }
}
