
mod authkm;
mod authkm_grpc;
mod contentkm;
mod contentkm_grpc;

pub use self::authkm::{
    UserCredentialsReq, AuthClaimsResp,
    UserEnrollReq, EnrollStatusResp
};

pub use self::authkm_grpc::{
    AuthKam, AuthKamClient, AuthKamServer
};

pub use self::contentkm::{
    Content, ContentHandles,
    Document, Document_Langue, Document_Niveau, Document_FiltreVisuel,
    Media, Media_Categorie, MetaData, Identification, TimeStamps
};

pub use self::contentkm_grpc::{
    ContentKam, ContentKamClient, ContentKamServer
};
