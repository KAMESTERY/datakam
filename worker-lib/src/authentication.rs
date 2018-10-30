
use dal::{create_complete_user, User, UserAuthData};
use security::{jwt_encode, jwt_decode, check_password};
use validation::AuthTrait;

// Enroll
pub fn enroll(user_id: String, email: String, username: String, password: String) -> String {
    create_complete_user(
        user_id,
        email,
        username,
        password
    )
}

// Get Claims
pub fn get_claims(token: String) -> Option<UserAuthData> {
    let user_auth_data: Option<UserAuthData> = jwt_decode(token);
    user_auth_data
}

// Login
pub fn login(user_id: String, email: String, password: String) -> Option<String> {
    let user = User::get_user(user_id.clone(), email)?;
    let password_hash = user.clone().password_hash?;
    check_password(user_id, password_hash, password).ok()?;
    let token = jwt_encode(user.to_auth_data());
    token
}

// Authenticate
pub fn authenticate(user_id: String, email: String, password: String) -> Option<UserAuthData> {
    let user = User::get_user(user_id.clone(), email)?;
    let password_hash = user.clone().password_hash?;
    check_password(user_id, password_hash, password).ok()?;
    let mut user_auth_data = user.to_auth_data();
    user_auth_data.token = jwt_encode(user_auth_data.clone());
    Some(user_auth_data)
}

// Logout