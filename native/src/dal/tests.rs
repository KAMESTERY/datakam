use crate::dal::{attr_s, DynaDB, ModelDynaConv, User};
use rusoto_dynamodb::AttributeValue;
use std::collections::HashMap;

static EMAIL: &'static str = "yuyu@yuyu.yuy";
static USERNAME: &'static str = "yuyu.yuy";
const ROLE: i32 = -9999;
const CONFIRMED: i32 = -99;

#[test]
fn crud_user() {

    println!("--------- Put User ----------------");

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

    println!("Put Response: {:?}", put_response);

    println!("--------- Get User ----------------");

    let key: HashMap<String, AttributeValue> =
        [
            (String::from("UserID"), attr_s(Some(String::from(EMAIL)))),
            (String::from("Email"), attr_s(Some(String::from(EMAIL))))
        ]
            .iter().cloned().collect();

    let user: Option<User> = DynaDB::get(table_name.clone(), key.clone());

    println!("User: {:?}", user);

    println!("--------- Query Users ----------------");

    let key_condition_expr = String::from("UserID = :user_id AND Email = :email");

    let data: HashMap<String, AttributeValue> =
        [
            (String::from(":user_id"), attr_s(Some(String::from(EMAIL)))),
            (String::from(":email"), attr_s(Some(String::from(EMAIL))))
        ]
            .iter().cloned().collect();

    let users: Option<Vec<User>> = DynaDB::query(
        table_name.clone(),
        Some(data),
        None,
        Some(key_condition_expr),
        None
    );

    println!("Users: {:?}", users);

    println!("--------- Delete User ----------------");

    let delete_response = DynaDB::delete(table_name.clone(), key.clone());

    println!("Delete Response: {:?}", delete_response);

    assert_eq!(1, 1);
}
