
use std::collections::HashMap;
use juniper::{FieldResult, RootNode};
use dal::{
    DynaDB, create_complete_user,
    create_complete_thing, delete_complete_thing,
    User, UserProfile, UserGroup, UserAuthData,
    Thing, Data, LaChose
};
use validation::{
    AuthDataTrait,
    from_token,
    validate_token,
    GUEST,
    USER,
    CONTRIBUTE,
    MANAGE,
    ADMINISTER
};
use authentication as auth;

#[derive(GraphQLEnum)]
enum Episode {
    NewHope,
    Empire,
    Jedi,
}

#[derive(GraphQLObject)]
#[graphql(description = "A humanoid creature in the Star Wars universe")]
struct Human {
    id: String,
    name: String,
    appears_in: Vec<Episode>,
    home_planet: String,
}

#[derive(GraphQLInputObject)]
#[graphql(description = "A humanoid creature in the Star Wars universe")]
struct NewHuman {
    name: String,
    appears_in: Vec<Episode>,
    home_planet: String,
}

//TODO: Move things in domain (ThingInput and ThingOutput)
#[derive(GraphQLInputObject)]
#[graphql(description = "A GThing :-)")]
struct GThing {
    name: String,
    user_id: String,
    data: Vec<Vec<String>>
//    data: Vec<(String, String)>
}

impl GThing {
    fn data_as_hashmap(&self) -> Result<HashMap<String, String>, &'static str> {
        let mut h = HashMap::new();
        self.data.clone().into_iter().for_each(|v| {
            let k: String = v.get(0)
                .expect("No Key Provided")
                .to_string();
            let v: String = v.get(1)
                .expect("No Value Provided")
                .to_string();
            h.insert(k, v);
        });
        debug!("Transformed VecOfVec {:?} into Map {:?}", self.data, h);
        Ok(h)
    }
}

pub struct QueryRoot;

graphql_object!(QueryRoot: () |&self| {
    field greet(&executor, name: String) -> FieldResult<String> {
        Ok(format!("Hello {}", name))
    }
    field get_claims(token: String) -> FieldResult<Option<UserAuthData>> {
        let user_auth_data = auth::get_claims(token);
        Ok(user_auth_data)
    }
    field login(user_id: String, email: String, password: String) -> FieldResult<Option<String>> {
        let token = auth::login(user_id, email, password);
        Ok(token)
    }
    field get_user(token: String, user_id: String, email: String) -> FieldResult<Option<User>> {
        secured!(
            token,
            user_id.clone(),
            Ok(User::get_user(user_id, email))
        )
    }
    field get_userprofile(token: String, user_id: String) -> FieldResult<Option<UserProfile>> {
        secured!(
            token,
            user_id.clone(),
            Ok(UserProfile::get_userprofile(user_id))
        )
    }
    field get_usergroup(token: String, group_id: String, user_id: String) -> FieldResult<Option<UserGroup>> {
        secured!(
            token,
            user_id.clone(),
            Ok(UserGroup::get_usergroup(group_id, user_id))
        )
    }
    field get_lachose(token: String, name: String, user_id: String) -> FieldResult<Option<LaChose>> {
        secured!(
            token,
            user_id.clone(),
            Ok(LaChose::get_lachose(name, user_id))
        )
    }

    field get_les_choses(token: String, user_id: String, names: Vec<String>) -> FieldResult<Option<Vec<LaChose>>> {
        secured!(
            token,
            user_id.clone(),
            Ok(LaChose::get_les_choses(user_id, names))
        )
    }

    field list_dynamodb_tables(&executor) -> FieldResult<Option<Vec<String>>> {
        // Get the context from the executor.
        let tables = DynaDB::list_tables();
        Ok(tables)
    }
    field human(&executor, id: String) -> FieldResult<Human> {
        Ok(Human{
            id: "1234".to_owned(),
            name: "Luke".to_owned(),
            appears_in: vec![Episode::NewHope],
            home_planet: "Mars".to_owned(),
        })
    }
});

pub struct MutationRoot;

graphql_object!(MutationRoot: () |&self| {
    field createHuman(&executor, new_human: NewHuman) -> FieldResult<Human> {
        Ok(Human{
            id: "1234".to_owned(),
            name: new_human.name,
            appears_in: new_human.appears_in,
            home_planet: new_human.home_planet,
        })
    }
    field register(user_id: String, email: String, username: String, password: String) -> FieldResult<String> {
        Ok(auth::register(user_id, email, username, password))
    }
    field create_complete_user(token: String, user_id: String, email: String, username: String, password: String) -> FieldResult<Option<String>> {
        secured!(
            token,
            user_id.clone(),
            //TODO: How to return AttributeValue struct?
            // field create_user(user_id: String, email: String, username: String, password: String) -> FieldResult<Option<HashMap<String, AttributeValue>>> {
            Ok(Some(create_complete_user(user_id, email, username, password)))
        )
    }
    field create_complete_thing(token: String, thing: GThing) -> FieldResult<Option<String>> {
        let data: HashMap<String, String> = thing.data_as_hashmap()?;
        secured!(
            token,
            thing.user_id.clone(),
            Ok(Some(
                create_complete_thing(
                    thing.name,
                    thing.user_id,
                    data
                )
            ))
        )
    }

    field delete_complete_thing(token: String, name: String, user_id: String) -> FieldResult<Option<String>> {
        secured!(
            token,
            user_id.clone(),
            Ok(delete_complete_thing(name, user_id))
        )
    }
});

pub type Schema = RootNode<'static, QueryRoot, MutationRoot>;

pub fn create_schema() -> Schema {
    Schema::new(QueryRoot {}, MutationRoot {})
}
