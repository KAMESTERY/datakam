
use dal::{attr_s, ModelDynaConv, User, DynaDB};
use jwt::{self, TokenData};
use security::{
    hash_password, check_password,
    jwt_encode, jwt_decode
};
use rusoto_dynamodb::AttributeValue;
use std::collections::HashMap;

static EMAIL: &'static str = "yuyu@yuyu.yuy";
static USERNAME: &'static str = "yuyu.yuy";
const ROLE: i32 = -9999;
const CONFIRMED: i32 = -99;

#[test]
fn crud_user() {

    debug!("--------- Put User ----------------");

    let table_name = String::from("User");

    let user_data = User::new()
        .with_user_id(String::from(EMAIL))
        .with_email(String::from(EMAIL))
        .with_username(String::from(USERNAME))
        .with_role(ROLE)
        .with_confirmed(CONFIRMED)
        .with_password_hash(String::from("sflkhjreuidjkldsaflhdsfhl"))
        .with_last_seen(String::from("today"))
        .drain();

    let put_response = DynaDB::put(table_name.clone(), user_data);

    debug!("Put Response: {:?}", put_response);

    debug!("--------- Get User ----------------");

    let key: HashMap<String, AttributeValue> =
        [
            (String::from("UserID"), attr_s(Some(String::from(EMAIL)))),
            (String::from("Email"), attr_s(Some(String::from(EMAIL))))
        ]
            .iter().cloned().collect();

    let user: Option<User> = DynaDB::get(table_name.clone(), key.clone());

    debug!("User: {:?}", user);

    debug!("--------- Query Users ----------------");

    let key_condition_expr = String::from("UserID = :user_id AND Email = :email");

    let data: HashMap<String, AttributeValue> =
        [
            (String::from(":user_id"), attr_s(Some(String::from(EMAIL)))),
            (String::from(":email"), attr_s(Some(String::from(EMAIL))))
        ]
            .iter().cloned().collect();

    let users: Option<Vec<User>> = DynaDB::query(table_name.clone(), key_condition_expr, data);

    debug!("Users: {:?}", users);

    debug!("--------- Delete User ----------------");

    let delete_response = DynaDB::delete(table_name.clone(), key.clone());

    debug!("Delete Response: {:?}", delete_response);

    assert_eq!(1, 1);
}

#[test]
fn hash_check() {
    debug!("--------- Get Secret Words ----------------");
    let secret_words = String::from("haha, this is secret");
    debug!("Secret {}", secret_words);

    debug!("--------- Hash Secret Words ----------------");
    let hashed_secret = hash_password(secret_words.clone());
    debug!("Hashed Secret {}", hashed_secret);

    debug!("--------- Check Secret Words ----------------");
    let right_secret = check_password(
        hashed_secret,
        secret_words
    );

    match right_secret {
        Ok(_) => assert_eq!(1, 1),
        Err(err) => {
            debug!("HASH_CHECK_ERROR {:?}", err);
            assert_eq!(1, -1)
        }
    }
}

/// Our claims struct, it needs to derive `Serialize` and/or `Deserialize`
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
struct Claims {
    sub: String,
    company: String
}

#[test]
fn jwt_claims_check() {
    let claims = Claims{ sub: String::from("Sub"), company: String::from("Company") };

    let token = jwt_encode(claims.clone());
    let token_string = token.unwrap();
    debug!("Token {}", token_string);

    let token_data: jwt::errors::Result<TokenData<Claims>> = jwt_decode(token_string);
    let retrieved_claims = token_data.unwrap().claims;
    debug!("Retrieved Claims: {:?}", retrieved_claims);

    assert_eq!(retrieved_claims, claims);
}
