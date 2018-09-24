use jwt::{self, TokenData};
use security as sec;
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use std::marker::Sized;
// A trait that the Validate derive will impl
use validator::{Validate, ValidationError};

pub trait AuthTrait<T: AuthDataTrait> {
    fn to_auth_data(&self) -> T;
    fn from_auth_data(&mut self, auth_data: T) -> Self;
}

pub trait AuthDataTrait: Clone + Serialize + DeserializeOwned {
    fn to_token(&self) -> Option<String> {
        let new_token = sec::jwt_encode(self.clone());
        new_token
    }
    fn from_token(&self, token: Option<String>) -> Option<Self> {
        let claims: Self = sec::jwt_decode(token?)?;
        Some(self.clone())
    }
    fn validate_token(&self, token: Option<String>) -> Option<String> {
        let claims: Self = sec::jwt_decode(token?)?;
        let new_token = sec::jwt_encode(claims);
        new_token
    }
}
