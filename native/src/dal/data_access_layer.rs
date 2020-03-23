use futures::future::join_all;

use crate::dal::models::{
    create_documents, update_documents, delete_documents,
    create_media_items, update_media_items, delete_media_items,
    ThingOutput, DocumentInput, MediaInput, QueryInput
};

#[derive(Debug)]
pub enum InputData{
    Documents {documents: Vec<DocumentInput>},
    Media {media: Vec<MediaInput>}
}

#[derive(Debug)]
pub enum DeleteData{
    Documents {data: Vec<Vec<String>>},
    Media {data: Vec<Vec<String>>}
}

pub async fn query(queries: Vec<QueryInput>) -> Vec<ThingOutput> {

    let mut handles = vec![];

    for q in queries {
        handles.push(async move {
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
            ).await
        });
    }

    let res = join_all(handles).await;

    let results = res.into_iter().filter(|c| {
        c.is_some()
    }).map(|x| x.unwrap()).flat_map(|res| res).collect();

    results
}

pub async fn get(data: Vec<Vec<String>>) -> Option<Vec<ThingOutput>> {
    ThingOutput::get_les_choses(data).await
}

pub async fn create(payload: InputData) -> Vec<String> {
    match payload {
        InputData::Documents {documents} => create_documents(documents).await,
        InputData::Media {media} => create_media_items(media).await
    }
}

pub async fn update(payload: InputData) -> Vec<String> {
    match payload {
        InputData::Documents {documents} => update_documents(documents).await,
        InputData::Media {media} => update_media_items(media).await
    }
}

pub async fn delete(payload: DeleteData) -> Vec<String> {
    match payload {
        DeleteData::Documents {data} => delete_documents(data).await,
        DeleteData::Media {data} => delete_media_items(data).await
    }
}
