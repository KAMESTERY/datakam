
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Document {
    pub topic: Option<String>,
    pub document_id: Option<String>,
    pub user_id: Option<String>,
    pub tags: Option<Vec<String>>,
    pub score: Option<i32>,
    pub version: Option<i32>,
    pub created_at: Option<DateTime>,
    pub updated_at: Option<DateTime>,
    pub slug : Option<String>,
    pub title: Option<String>,
    pub identifier: Option<String>,
    pub body : Option<String>,
    pub publish: Option< Boolean>,
    pub filtre_visuel: Option<i32>,
    pub langue: Option<i32>,
    pub niveau: Option<i32>,
    pub  media: Option<Vec<Media>>
}
