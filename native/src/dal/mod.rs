
mod dynatraits;
mod dynamodb;
mod models;
mod data_access_layer;

pub use self::dynatraits::{
    DeleteItemInputBuilder, ModelDynaConv, AttributeValueBuilder,
    GetItemInputBuilder, QueryInputBuilder, ScanInputBuilder, PutItemInputBuilder,
    BatchGetItemInputBuilder,  BatchWriteItemInputBuilder
};
pub use self::dynamodb::{
    attr_b, attr_bool, attr_bs, attr_l, attr_m,
    attr_n, attr_ns, attr_null, attr_s, attr_ss,
    DynaDB, QueryParams, ScanParams
};
pub use self::models::{
    create_complete_user, create_complete_thing, delete_complete_thing,
    User, UserProfile, UserGroup, UserAuthData,
    Thing, Data, ThingDataTrait, ThingInput, ThingOutput,
    DocumentInput, MediaInput, QueryInput,
    DocumentRef, Document, DocStreamRef, DocStream
};
pub use self::data_access_layer::{
    query, scan, get, create, update, delete,
    Content, ContentDocOrStream, ContentRef, ContentRefDocOrStream
};

#[cfg(test)] mod tests;
