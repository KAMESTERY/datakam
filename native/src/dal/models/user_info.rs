use std::collections::HashMap;

use rusoto_dynamodb::AttributeValue;

use crate::dal::dynamodb::{attr_n, attr_s, DynaDB};
use crate::dal::dynatraits::ModelDynaConv;
use crate::security as sec;
use crate::validation::{AuthDataTrait, AuthTrait};
use crate::dal::models::constants::*;

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct UserAuthData {
    pub token: Option<String>,
    pub user_id: Option<String>,
    pub email: Option<String>,
    pub role: Option<i32>
}

impl AuthDataTrait for UserAuthData {
    fn get_id(&self) -> String {
        self.clone().user_id.unwrap_or(String::from(""))
    }

    fn get_role(&self) -> i32 {
        self.clone().role.unwrap_or(0x0).to_owned()
    }
}

pub async fn create_complete_user(user_id: String, email: String, username: String, password: String) -> String {
    match User::get_user(user_id.clone(), email.clone()).await {
        Some(_user) => {
            let err_msg = String::from("");
            debug!("User Already Exists! {}", user_id.clone());
            err_msg
        },
        None => {
            let password_hash = sec::hash_password(user_id.clone(), password);
            User::create_user(
                user_id.clone(),
                email,
                username,
                password_hash
            ).await;

            UserProfile::create_userprofile(
                user_id.clone(),
                String::from("spiuewropiewqrfhjdnlasjhdsiahudfas"),
                String::from("What is your name?"),
                9999,
                String::from("What is it about you?"),
                String::from("What is your location?"),
                String::from("What is your location?"),
            ).await;

            UserGroup::create_usergroup(
                String::from("user"),
                user_id,
                String::from("user")
            ).await;

            debug!("Fully created User, Profile, and Group :-)");

            String::from("SUCCESS")
        }
    }
}

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct User {
    pub user_id: Option<String>,
    pub email: Option<String>,
    pub username: Option<String>,
    pub role: Option<i32>,
    pub confirmed: Option<i32>,
    pub password_hash: Option<String>,
    pub last_seen: Option<String>
}

impl User {
    // API Functions
    pub async fn create_user(user_id: String, email: String, username: String, password_hash: String) -> Option<HashMap<String, AttributeValue>> {
        // TODO: Create Profile and Group(s) as well
        let user_data = User::default()
            .with_user_id(user_id)
            .with_email(email)
            .with_username(username)
            .with_role(-9999)
            .with_confirmed(-99)
            .with_password_hash(password_hash)
            .with_last_seen(String::from("today"))
            .drain();

        debug!("Put User Data: {:?}", user_data);

        let put_response = DynaDB::put(USER_TABLE.into(), user_data).await;

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub async fn get_user(user_id: String, email: String) -> Option<User> {
        //TODO: Retrieve Profile and Groups as well
        let key = User::default()
            .with_user_id(user_id)
            .with_email(email)
            .key();

        debug!("Get User Key: {:?}", key);

        let user: Option<User> = DynaDB::get(USER_TABLE.into(), key.clone()).await;

        debug!("User: {:?}", user);

        user
    }
    // Builder Function
    pub fn with_user_id(&mut self, user_id: String) -> Self {
        self.user_id = Some(user_id);
        self.clone()
    }
    pub fn with_email(&mut self, email: String) -> Self {
        self.email = Some(email);
        self.clone()
    }
    pub fn with_username(&mut self, username: String) -> Self {
        self.username = Some(username);
        self.clone()
    }
    pub fn with_role(&mut self, role: i32) -> Self {
        self.role = Some(role);
        self.clone()
    }
    pub fn with_confirmed(&mut self, confirmed: i32) -> Self {
        self.confirmed = Some(confirmed);
        self.clone()
    }
    pub fn with_password_hash(&mut self, password_hash: String) -> Self {
        self.password_hash = Some(password_hash);
        self.clone()
    }
    pub fn with_last_seen(&mut self, last_seen: String) -> Self {
        self.last_seen = Some(last_seen);
        self.clone()
    }
}

impl ModelDynaConv for User {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                USER_ID => self.user_id = value.s,
                EMAIL => self.email = value.s,
                USERNAME => self.username = value.s,
                ROLE => self.role = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                CONFIRMED => self.confirmed = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                PASSWORD_HASH => self.password_hash = value.s,
                LAST_SEEN => self.last_seen = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (USER_ID.into(), attr_s(self.user_id)),
                (EMAIL.into(), attr_s(self.email)),
                (USERNAME.into(), attr_s(self.username)),
                (ROLE.into(), attr_n(self.role)),
                (CONFIRMED.into(), attr_n(self.confirmed)),
                (PASSWORD_HASH.into(), attr_s(self.password_hash)),
                (LAST_SEEN.into(), attr_s(self.last_seen))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (USER_ID.into(), attr_s(self.user_id)),
                (EMAIL.into(), attr_s(self.email))
            ]
                .iter().cloned().collect();
        key
    }
}

impl AuthTrait<UserAuthData> for User {
    fn to_auth_data(&self) -> UserAuthData {
        UserAuthData{
            token: None,
            user_id: self.user_id.to_owned(),
            email: self.email.to_owned(),
            role: self.role.to_owned()
        }
    }

    fn from_auth_data(&mut self, auth_data: UserAuthData) -> User {
        let user = User::default()
            .with_user_id(auth_data.clone().user_id.unwrap())
            .with_username(auth_data.email.unwrap());
        user
    }
}

#[derive(Default, Clone, Debug)]
pub struct UserProfile {
    pub user_id: Option<String>,
    pub avatar_hash: Option<String>,
    pub name: Option<String>,
    pub age: Option<i32>,
    pub about_me: Option<String>,
    pub location: Option<String>,
    pub member_since: Option<String>
}

impl UserProfile {
    // API Functions
    pub async fn create_userprofile(user_id: String, avatar_hash: String, name: String, age: i32, about_me: String, location: String, member_since: String) -> Option<HashMap<String, AttributeValue>>  {
        let userprofile_data = UserProfile::default()
            .with_user_id(user_id)
            .with_avatar_hash(avatar_hash)
            .with_name(name)
            .with_age(age)
            .with_about_me(about_me)
            .with_location(location)
            .with_member_since(member_since)
            .drain();

        debug!("Put UserProfile Data: {:?}", userprofile_data);

        let put_response = DynaDB::put(USER_PROFILE_TABLE.into(), userprofile_data).await;

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub async fn get_userprofile(user_id: String) -> Option<UserProfile> {
        let key = UserProfile::default()
            .with_user_id(user_id)
            .key();

        debug!("Get User Key: {:?}", key);

        let user_profile: Option<UserProfile> = DynaDB::get(USER_PROFILE_TABLE.into(), key.clone()).await;

        debug!("UserProfile: {:?}", user_profile);

        user_profile
    }
    // Builder Function
    pub fn with_user_id(&mut self, user_id: String) -> Self {
        self.user_id = Some(user_id);
        self.clone()
    }
    pub fn with_avatar_hash(&mut self, avatar_hash: String) -> Self {
        self.avatar_hash = Some(avatar_hash);
        self.clone()
    }
    pub fn with_name(&mut self, name: String) -> Self {
        self.name = Some(name);
        self.clone()
    }
    pub fn with_age(&mut self, age: i32) -> Self {
        self.age = Some(age);
        self.clone()
    }
    pub fn with_about_me(&mut self, about_me: String) -> Self {
        self.about_me = Some(about_me);
        self.clone()
    }
    pub fn with_location(&mut self, location: String) -> Self {
        self.location = Some(location);
        self.clone()
    }
    pub fn with_member_since(&mut self, member_since: String) -> Self {
        self.member_since = Some(member_since);
        self.clone()
    }
}

impl ModelDynaConv for UserProfile {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                USER_ID => self.user_id = value.s,
                AVATAR_HASH => self.avatar_hash = value.s,
                NAME => self.name = value.s,
                AGE => self.age = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                ABOUT_ME => self.about_me = value.s,
                LOCATION => self.location = value.s,
                MEMBER_SINCE => self.member_since = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (USER_ID.into(), attr_s(self.user_id)),
                (AVATAR_HASH.into(), attr_s(self.avatar_hash)),
                (NAME.into(), attr_s(self.name)),
                (AGE.into(), attr_n(self.age)),
                (ABOUT_ME.into(), attr_s(self.about_me)),
                (LOCATION.into(), attr_s(self.location)),
                (MEMBER_SINCE.into(), attr_s(self.member_since))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (USER_ID.into(), attr_s(self.user_id))
            ]
                .iter().cloned().collect();
        key
    }
}

#[derive(Default, Clone, Debug)]
pub struct UserGroup {
    pub group_id: Option<String>,
    pub user_id: Option<String>,
    pub name: Option<String>
}

impl UserGroup {
    // API Functions
    pub async fn qet_usergroups_by_userid(user_id: String) -> Option<Vec<UserGroup>> {

        let expr_attr_names: HashMap<String, String> =
            [
                (String::from("#UserID"), USER_ID.into())
            ].iter().cloned().collect();
        let data: HashMap<String, AttributeValue> =
            [
                (String::from(":UserID"), attr_s(Some(user_id)))
            ].iter().cloned().collect();

        let usergroups = DynaDB::scan(
            USER_GROUPS_TABLE.into(),
            Some(String::from("UserIDIndex")),
            Some(expr_attr_names),
            Some(data),
            Some(String::from("#UserID = :UserID")),
            None,
            None
        ).await;

        dbg!(&usergroups);

        usergroups
    }
    pub async fn create_usergroup(group_id: String, user_id: String, name: String) -> Option<HashMap<String, AttributeValue>>  {
        let usergroup_data = UserGroup::default()
            .with_group_id(group_id)
            .with_user_id(user_id)
            .with_name(name)
            .drain();

        debug!("Put UserGroup Data: {:?}", usergroup_data);

        let put_response = DynaDB::put(USER_GROUPS_TABLE.into(), usergroup_data).await;

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub async fn get_usergroup(group_id: String, user_id: String) -> Option<UserGroup> {
        let key = UserGroup::default()
            .with_group_id(group_id)
            .with_user_id(user_id)
            .key();

        debug!("Get UserGroup Key: {:?}", key);

        let user_group: Option<UserGroup> = DynaDB::get(USER_GROUPS_TABLE.into(), key.clone()).await;

        debug!("UserGroup: {:?}", user_group);

        user_group
    }
    pub fn with_user_id(&mut self, user_id: String) -> Self {
        self.user_id = Some(user_id);
        self.clone()
    }
    pub fn with_group_id(&mut self, group_id: String) -> Self {
        self.group_id = Some(group_id);
        self.clone()
    }
    pub fn with_name(&mut self, name: String) -> Self {
        self.name = Some(name);
        self.clone()
    }
}

impl ModelDynaConv for UserGroup {
    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                "GroupID" => self.group_id = value.s,
                "UserID" => self.user_id = value.s,
                "Name" => self.name = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (GROUP_ID.into(), attr_s(self.group_id)),
                (USER_ID.into(), attr_s(self.user_id)),
                (NAME.into(), attr_s(self.name))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (GROUP_ID.into(), attr_s(self.group_id)),
                (USER_ID.into(), attr_s(self.user_id))
            ]
                .iter().cloned().collect();
        key
    }
}
