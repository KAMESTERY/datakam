
mod authvanille;
mod contentvanille;

pub use self::authvanille::{
    UserCredentialsReq, AuthClaimsResp,
    UserEnrollReq, EnrollStatusResp
};

pub use self::contentvanille::{
    Content, ContentHandles,
    Document, Document_Langue, Document_Niveau, Document_FiltreVisuel,
    Media, Media_Categorie, MetaData, Identification, TimeStamps
};
