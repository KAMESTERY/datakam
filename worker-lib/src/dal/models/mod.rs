
//mod dynatraits;
//modf self::pa
mod things;
mod user_info;

pub use self::user_info::{
    create_complete_user,
    User, UserProfile, UserGroup, UserAuthData
};
pub use self::things::{
    create_documents, update_documents, delete_documents,
    create_complete_thing, delete_complete_thing,
    Thing, Data, ThingDataTrait, DocumentInput, ThingInput, ThingOutput
//    , ContentList
};

#[cfg(test)] mod tests;
