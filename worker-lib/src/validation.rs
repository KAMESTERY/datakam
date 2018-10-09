use jwt::{self, TokenData};
use security as sec;
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use std::marker::Sized;
// A trait that the Validate derive will impl
use validator::{Validate, ValidationError};

pub const GUEST: u32 = 0x01;
pub const USER: u32 = 0x02;
pub const CONTRIBUTE: u32 = 0x04;
pub const MANAGE: u32 = 0x08;
pub const ADMINISTER: u32 = 0x80;

pub trait AuthTrait<T: AuthDataTrait> {
    fn to_auth_data(&self) -> T;
    fn from_auth_data(&mut self, auth_data: T) -> Self;
}

pub trait AuthDataTrait: Clone + Serialize + DeserializeOwned {
    fn to_token(&self) -> Option<String> {
        let new_token = sec::jwt_encode(self.clone());
        new_token
    }
    fn get_id(&self) -> String;
    fn get_role(&self) -> i32;
    fn can(&self, id: String, role: i32) -> bool {
        let allowed = self.get_id() == id || self.get_role() >= role;
        allowed
    }
}

pub fn from_token<T: AuthDataTrait>(token: String) -> Option<T> {
    let claims: T = sec::jwt_decode(token)?;
    Some(claims)
}

pub fn validate_token<T: AuthDataTrait>(token: String) -> Option<String> {
    let claims: T = sec::jwt_decode(token)?;
    let new_token = sec::jwt_encode(claims);
    new_token
}
