
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use std::marker::Sized;
use jwt::{self, TokenData};
// A trait that the Validate derive will impl
use validator::{Validate, ValidationError};
use security as sec;

#[derive(Clone, Deserialize, Serialize)]
pub struct AuthData {
    pub user_id: Option<String>,
    pub email: Option<String>
}

pub trait AuthTrait {
    fn to_auth_data(&self) -> AuthData;
    fn from_auth_data(&mut self, auth_data: AuthData) -> Self;
}

pub fn validate_token(token: Option<String>) -> Option<String> {
    let token_data: TokenData<AuthData> = sec::jwt_decode(token?).ok()?;
    let new_token = sec::jwt_encode(token_data.claims).ok();
    new_token
}
