
use rusoto_dynamodb::{AttributeValue};
use std::collections::HashMap;
use dal::dynatraits::{ModelDynaConv};
use dal::dynamodb::{attr_n, attr_s, DynaDB};
use security as sec;
use validation as val;
use validation::{AuthData, AuthTrait};

pub fn create_complete_user(user_id: String, email: String, username: String, password: String) -> String {
    let password_hash = sec::hash_password(password);
    User::create_user(
        user_id.clone(),
        email,
        username,
        password_hash
    );

    UserProfile::create_userprofile(
        user_id.clone(),
        String::from("spiuewropiewqrfhjdnlasjhdsiahudfas"),
        String::from("What is your name?"),
        9999,
        String::from("What is it about you?"),
        String::from("What is your location?"),
        String::from("What is your location?"),
    );

    UserGroup::create_usergroup(
        String::from("user"),
        user_id,
        String::from("user")
    );

    String::from("SUCCESS")
}

#[derive(Clone, Debug, Serialize, Deserialize, GraphQLObject)]
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
    pub fn new() -> Self {
        User{
            user_id: None,
            email: None,
            username: None,
            role: None,
            confirmed: None,
            password_hash: None,
            last_seen: None,
        }
    }
    // API Functions
    pub fn create_user(user_id: String, email: String, username: String, password_hash: String) -> Option<HashMap<String, AttributeValue>> {
        // TODO: Create Profile and Group(s) as well
        let user_data = User::new()
            .with_user_id(user_id)
            .with_email(email)
            .with_username(username)
            .with_role(-9999)
            .with_confirmed(-99)
            .with_password_hash(password_hash)
            .with_last_seen(String::from("today"))
            .drain();

        debug!("Put User Data: {:?}", user_data);

        let put_response = DynaDB::put(String::from("User"), user_data);

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub fn get_user(user_id: String, email: String) -> Option<User> {
        //TODO: Retrieve Profile and Groups as well
        let key = User::new()
            .with_user_id(user_id)
            .with_email(email)
            .key();

        debug!("Get User Key: {:?}", key);

        let user: Option<User> = DynaDB::get(String::from("User"), key.clone());

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
    fn new() -> Self {
        User{
            user_id: None,
            email: None,
            username: None,
            role: None,
            confirmed: None,
            password_hash: None,
            last_seen: None,
        }
    }

    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                "UserID" => self.user_id = value.s,
                "Email" => self.email = value.s,
                "Username" => self.username = value.s,
                "Role" => self.role = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                "Confirmed" => self.confirmed = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                "PasswordHash" => self.password_hash = value.s,
                "LastSeen" => self.last_seen = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (String::from("UserID"), attr_s(self.user_id)),
                (String::from("Email"), attr_s(self.email)),
                (String::from("Username"), attr_s(self.username)),
                (String::from("Role"), attr_n(self.role)),
                (String::from("Confirmed"), attr_n(self.confirmed)),
                (String::from("PasswordHash"), attr_s(self.password_hash)),
                (String::from("LastSeen"), attr_s(self.last_seen))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (String::from("UserID"), attr_s(Some(self.user_id.unwrap_or(String::from("NoUserID"))))),
                (String::from("Email"), attr_s(Some(self.email.unwrap_or(String::from("NoEmail")))))
            ]
                .iter().cloned().collect();
        key
    }
}

impl AuthTrait for User {
    fn to_auth_data(&self) -> AuthData {
        AuthData{
            user_id: self.user_id.to_owned(),
            email: self.email.to_owned()
        }
    }

    fn from_auth_data(&mut self, auth_data: AuthData) -> User {
        let user = User::new()
            .with_user_id(auth_data.clone().user_id.unwrap())
            .with_username(auth_data.email.unwrap());
        user
    }
}

#[derive(Clone, Debug, GraphQLObject)]
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
    fn new() -> Self {
        UserProfile {
            user_id: None,
            avatar_hash: None,
            name: None,
            age: None,
            about_me: None,
            location: None,
            member_since: None,
        }
    }
    // API Functions
    pub fn create_userprofile(user_id: String, avatar_hash: String, name: String, age: i32, about_me: String, location: String, member_since: String) -> Option<HashMap<String, AttributeValue>>  {
        let userprofile_data = UserProfile::new()
            .with_user_id(user_id)
            .with_avatar_hash(avatar_hash)
            .with_name(name)
            .with_age(age)
            .with_about_me(about_me)
            .with_location(location)
            .with_member_since(member_since)
            .drain();

        debug!("Put UserProfile Data: {:?}", userprofile_data);

        let put_response = DynaDB::put(String::from("UserProfile"), userprofile_data);

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub fn get_userprofile(user_id: String) -> Option<UserProfile> {
        let key = UserProfile::new()
            .with_user_id(user_id)
            .key();

        debug!("Get User Key: {:?}", key);

        let user_profile: Option<UserProfile> = DynaDB::get(String::from("UserProfile"), key.clone());

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
    fn new() -> Self {
        UserProfile {
            user_id: None,
            avatar_hash: None,
            name: None,
            age: None,
            about_me: None,
            location: None,
            member_since: None,
        }
    }

    fn hydrate(&mut self, dyna_data: HashMap<String, AttributeValue>) -> Self {
        for (key, value) in dyna_data {
            match key.as_ref() {
                "UserID" => self.user_id = value.s,
                "AvatarHash" => self.avatar_hash = value.s,
                "Name" => self.name = value.s,
                "Age" => self.age = value.n.and_then(|s| Some(s.parse::<i32>().unwrap_or(0))),
                "AboutMe" => self.about_me = value.s,
                "Location" => self.location = value.s,
                "MemberSince" => self.member_since = value.s,
                _ => warn!("Unexpected Data: [{} => {:?}]", key, value)
            }
        }
        self.clone()
    }

    fn drain(self) -> HashMap<String, AttributeValue> {
        let data: HashMap<String, AttributeValue> =
            [
                (String::from("UserID"), attr_s(self.user_id)),
                (String::from("AvatarHash"), attr_s(self.avatar_hash)),
                (String::from("Name"), attr_s(self.name)),
                (String::from("Age"), attr_n(self.age)),
                (String::from("AboutMe"), attr_s(self.about_me)),
                (String::from("Location"), attr_s(self.location)),
                (String::from("MemberSince"), attr_s(self.member_since))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (String::from("UserID"), attr_s(Some(self.user_id.unwrap_or(String::from("NoUserID")))))
            ]
                .iter().cloned().collect();
        key
    }
}

#[derive(Clone, Debug, GraphQLObject)]
pub struct UserGroup {
    pub group_id: Option<String>,
    pub user_id: Option<String>,
    pub name: Option<String>
}

impl UserGroup {
    pub fn new() -> Self {
        UserGroup {
            group_id: None,
            user_id: None,
            name: None,
        }
    }
    // API Functions
    pub fn create_usergroup(group_id: String, user_id: String, name: String) -> Option<HashMap<String, AttributeValue>>  {
        let usergroup_data = UserGroup::new()
            .with_group_id(group_id)
            .with_user_id(user_id)
            .with_name(name)
            .drain();

        debug!("Put UserGroup Data: {:?}", usergroup_data);

        let put_response = DynaDB::put(String::from("UserGroups"), usergroup_data);

        debug!("Put Response: {:?}", put_response);

        put_response
    }
    pub fn get_usergroup(group_id: String, user_id: String) -> Option<UserGroup> {
        let key = UserGroup::new()
            .with_group_id(group_id)
            .with_user_id(user_id)
            .key();

        debug!("Get UserGroup Key: {:?}", key);

        let user_group: Option<UserGroup> = DynaDB::get(String::from("UserGroups"), key.clone());

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
    fn new() -> Self {
        UserGroup {
            group_id: None,
            user_id: None,
            name: None,
        }
    }

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
                (String::from("GroupID"), attr_s(self.group_id)),
                (String::from("UserID"), attr_s(self.user_id)),
                (String::from("Name"), attr_s(self.name))
            ]
                .iter().cloned().collect();
        data
    }

    fn key(self) -> HashMap<String, AttributeValue> {
        let key: HashMap<String, AttributeValue> =
            [
                (String::from("GroupID"), attr_s(Some(self.group_id.unwrap_or(String::from("NoGroupID"))))),
                (String::from("UserID"), attr_s(Some(self.user_id.unwrap_or(String::from("NoUserID")))))
            ]
                .iter().cloned().collect();
        key
    }
}
