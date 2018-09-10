
use std::collections::HashMap;
use juniper::{FieldResult, RootNode};
use dal::{
    DynaDB, create_complete_user,
    create_complete_thing, delete_complete_thing,
    User, UserProfile, UserGroup,
    Thing, Data, LaChose
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
    field login(user_id: String, email: String, password: String) -> FieldResult<Option<String>> {
        let token = auth::login(user_id, email, password);
        Ok(token)
    }
    field get_user(user_id: String, email: String) -> FieldResult<Option<User>> {
        let user = User::get_user(user_id, email);
        Ok(user)
    }
    field get_userprofile(user_id: String) -> FieldResult<Option<UserProfile>> {
        let user_profile = UserProfile::get_userprofile(user_id);
        Ok(user_profile)
    }
    field get_usergroup(group_id: String, user_id: String) -> FieldResult<Option<UserGroup>> {
        let user_group = UserGroup::get_usergroup(group_id, user_id);
        Ok(user_group)
    }
    field get_lachose(name: String, user_id: String) -> FieldResult<Option<LaChose>> {
        let la_chose = LaChose::get_lachose(name, user_id);
        Ok(la_chose)
    }

    field get_les_choses(user_id: String, names: Vec<String>) -> FieldResult<Option<Vec<LaChose>>> {
        let les_choses = LaChose::get_les_choses(user_id, names);
        Ok(les_choses)
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
    field create_complete_user(user_id: String, email: String, username: String, password: String) -> FieldResult<String> {
    //TODO: How to return AttributeValue struct?
//    field create_user(user_id: String, email: String, username: String, password: String) -> FieldResult<Option<HashMap<String, AttributeValue>>> {

        let put_response = create_complete_user(user_id, email, username, password);

        debug!("Put Response: {:?}", put_response);

        Ok(String::from("SUCCESS"))
//        Ok(put_response)
    }
    field create_complete_thing(thing: GThing) -> FieldResult<String> {
        let data: HashMap<String, String> = thing.data_as_hashmap()?;
//        let data: HashMap<String, String> = thing.data.iter().cloned().collect();
        let put_response = create_complete_thing(
            thing.name,
            thing.user_id,
            data
        );

        debug!("Put Response: {:?}", put_response);

        Ok(String::from("SUCCESS"))
    }

    field delete_complete_thing(name: String, user_id: String) -> FieldResult<Option<String>> {
        let result = delete_complete_thing(name, user_id);
        Ok(result)
    }
});

pub type Schema = RootNode<'static, QueryRoot, MutationRoot>;

pub fn create_schema() -> Schema {
    Schema::new(QueryRoot {}, MutationRoot {})
}
