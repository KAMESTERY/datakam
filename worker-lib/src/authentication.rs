
use dal::{create_complete_user, User};
use security::{jwt_encode, jwt_decode, check_password};
use validation::AuthTrait;

// Register
pub fn register(user_id: String, email: String, username: String, password: String) -> String {
    create_complete_user(
        user_id,
        email,
        username,
        password
    )
}

// Login
pub fn login(user_id: String, email: String, password: String) -> Option<String> {
    let user = User::get_user(user_id, email)?;
    let password_hash = user.clone().password_hash?;
    check_password(password_hash, password).ok()?;
    let token = jwt_encode(user.to_auth_data())?;
    Some(token)
}

// Logout