
use rayon::prelude::*;

use crate::dal::models::{
    create_documents, update_documents, delete_documents,
    create_media_items, update_media_items, delete_media_items,
    ThingOutput, DocumentInput, MediaInput, QueryInput
};

pub enum InputData{
    Documents {documents: Vec<DocumentInput>},
    Media {media: Vec<MediaInput>}
}

pub enum DeleteData{
    Documents {data: Vec<Vec<String>>},
    Media {data: Vec<Vec<String>>}
}

pub fn query(queries: Vec<QueryInput>) -> Option<Vec<ThingOutput>> {
    let results = queries.par_iter().map(|q| {
        let limit64 = match q.limit {
            Some(l) => Some(i64::from(l)),
            None => None
        };
        ThingOutput::query_les_choses(
            q.index_name.clone(),
            q.attr_names.clone(),
            q.filter_expr.clone(),
            q.key_condition_expr.clone(),
            q.projection_expr.clone(),
            q.select.clone(),
            limit64,
            q.raw_data.clone()
        )
    }).filter(|c| {
        c.is_some()
    }).map(|x| x.unwrap()).flat_map(|res| res).collect();
    Some(results)
}

pub fn get(data: Vec<Vec<String>>) -> Option<Vec<ThingOutput>> {
    ThingOutput::get_les_choses(data)
}

pub fn create(payload: InputData) -> Vec<String> {
    match payload {
        InputData::Documents {documents} => create_documents(documents),
        InputData::Media {media} => create_media_items(media)
    }
}

pub fn update(payload: InputData) -> Vec<String> {
    match payload {
        InputData::Documents {documents} => update_documents(documents),
        InputData::Media {media} => update_media_items(media)
    }
}

pub fn delete(payload: DeleteData) -> Vec<String> {
    match payload {
        DeleteData::Documents {data} => delete_documents(data),
        DeleteData::Media {data} => delete_media_items(data)
    }
}
