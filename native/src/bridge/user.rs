use neon::prelude::*;
use rayon::prelude::*;
use std::sync::Arc;
use tokio::runtime::Runtime;
use tokio::sync::Barrier;
use futures::future::join_all;

use crate::authentication;
use crate::dal::{User, UserAuthData, UserGroup, UserProfile};

// User

pub fn enroll(mut cx: FunctionContext) -> JsResult<JsString> {
    let user_obj = cx.argument::<JsObject>(0)?
        .downcast::<JsObject>()
        .unwrap_or(JsObject::new(&mut cx));

    let user_id = get_js_val!(user_obj, cx, "UserID", JsString, cx.string(""));
    let email = get_js_val!(user_obj, cx, "Email", JsString, cx.string(""));
    let username = get_js_val!(user_obj, cx, "Username", JsString, cx.string(""));
    let password = get_js_val!(user_obj, cx, "Password", JsString, cx.string(""));

    // let user_id = cx.argument::<JsString>(0)?.value();
    // let email = cx.argument::<JsString>(1)?.value();
    // let username = cx.argument::<JsString>(1)?.value();
    // let password = cx.argument::<JsString>(2)?.value();

    let status = authentication::enroll(user_id, email, username, password);

    dbg!(&status);

    Ok(cx.string(status))
}

pub fn authenticate(mut cx: FunctionContext) -> JsResult<JsObject> {
    let creds_obj = cx.argument::<JsObject>(0)?
        .downcast::<JsObject>()
        .unwrap_or(JsObject::new(&mut cx));

    let user_id = get_js_val!(creds_obj, cx, "UserID", JsString, cx.string(""));
    let email = get_js_val!(creds_obj, cx, "Email", JsString, cx.string(""));
    let password = get_js_val!(creds_obj, cx, "Password", JsString, cx.string(""));

    // let user_id = cx.argument::<JsString>(0)?.value();
    // let email = cx.argument::<JsString>(1)?.value();
    // let password = cx.argument::<JsString>(2)?.value();

    let object = JsObject::new(&mut cx);

    match authentication::authenticate(user_id, email, password) {
        Some(user_auth_data) => {
            dbg!(&user_auth_data);

            let token = cx.string(user_auth_data.token.unwrap_or_default());
            let user_id = cx.string(user_auth_data.user_id.unwrap_or_default());
            let email = cx.string(user_auth_data.email.unwrap_or_default());
            let role = cx.number(user_auth_data.role.unwrap_or_default());

            object.set(&mut cx, "Token", token).unwrap();
            object.set(&mut cx, "UserId", user_id).unwrap();
            object.set(&mut cx, "Email", email).unwrap();
            object.set(&mut cx, "Role", role).unwrap();
        }
        None => {}
    }

    Ok(object)
}

pub fn get_userinfo(mut cx: FunctionContext) -> JsResult<JsObject> {
    let user_ref_obj = cx.argument::<JsObject>(0)?
        .downcast::<JsObject>()
        .unwrap_or(JsObject::new(&mut cx));

    let user_id = get_js_val!(user_ref_obj, cx, "UserID", JsString, cx.string(""));
    let email = get_js_val!(user_ref_obj, cx, "Email", JsString, cx.string(""));

    let user_id_ref = Arc::new(user_id);
    let email_ref = Arc::new(email);

    // let user_id_ref = Arc::new(cx.argument::<JsString>(0)?.value());
    // let email_ref = Arc::new(cx.argument::<JsString>(1)?.value());

    let mut rt = Runtime::new().unwrap();

    let user_id = user_id_ref.clone();
    let email = email_ref.clone();
    let user_handle = rt.spawn(async move {
        User::get_user(user_id.to_string(), email.to_string()).await
    });

    let user_id = user_id_ref.clone();
    let usergroups_handle = rt.spawn(async move {
        UserGroup::qet_usergroups_by_userid(user_id.to_string()).await
    });

    let user_id = user_id_ref.clone();
    let userprofile_handle = rt.spawn(async move {
        UserProfile::get_userprofile(user_id.to_string()).await
    });

    rt.block_on(async move {
        let object = JsObject::new(&mut cx);

        let user_opt = user_handle.await;
        if user_opt.is_ok() {
            match user_opt.unwrap() {
                Some(user) => {
                    dbg!(&user);

                    let user_id = cx.string(user.user_id.unwrap_or_default());
                    let email = cx.string(user.email.unwrap_or_default());
                    let username = cx.string(user.username.unwrap_or_default());
                    let role = cx.number(user.role.unwrap_or_default());
                    let confirmed = cx.number(user.confirmed.unwrap_or_default());
                    let last_seen = cx.string(user.last_seen.unwrap_or_default());

                    object.set(&mut cx, "UserID", user_id).unwrap();
                    object.set(&mut cx, "Email", email).unwrap();
                    object.set(&mut cx, "Username", username).unwrap();
                    object.set(&mut cx, "Role", role).unwrap();
                    object.set(&mut cx, "Confirmed", confirmed).unwrap();
                    object.set(&mut cx, "LastSeen", last_seen).unwrap();
                }
                None => {}
            }
        }

        let usergroups_opt = usergroups_handle.await;
        if usergroups_opt.is_ok() {
            match usergroups_opt.unwrap() {
                Some(usergroups) => {
                    dbg!(&usergroups);

                    let groups_array = JsArray::new(&mut cx, usergroups.len() as u32);
                    for (i, group) in usergroups.iter().enumerate() {
                        let group_object = JsObject::new(&mut cx);

                        // let user_id = cx.string(group.user_id.unwrap_or_default());
                        let group_id = cx.string(group.group_id.clone().unwrap_or_default());
                        let name = cx.string(group.name.clone().unwrap_or_default());

                        // object.set(&mut cx, "userId", user_id).unwrap();
                        group_object.set(&mut cx, "GroupId", group_id).unwrap();
                        group_object.set(&mut cx, "GroupName", name).unwrap();

                        groups_array.set(&mut cx, i as u32, group_object).unwrap();
                    }

                    object.set(&mut cx, "Groups", groups_array).unwrap();
                }
                None => {}
            }
        }

        let userprofile_opt = userprofile_handle.await;
        if userprofile_opt.is_ok() {
            match userprofile_opt.unwrap() {
                Some(userprofile) => {
                    dbg!(&userprofile);

                    // let user_id = cx.string(userprofile.user_id.unwrap_or_default());
                    let avatar_hash = cx.string(userprofile.avatar_hash.unwrap_or_default());
                    let name = cx.string(userprofile.name.unwrap_or_default());
                    let age = cx.number(userprofile.age.unwrap_or_default());
                    let about_me = cx.string(userprofile.about_me.unwrap_or_default());
                    let location = cx.string(userprofile.location.unwrap_or_default());
                    let member_since = cx.string(userprofile.member_since.unwrap_or_default());

                    // object.set(&mut cx, "userId", user_id).unwrap();
                    object.set(&mut cx, "AvatarHash", avatar_hash).unwrap();
                    object.set(&mut cx, "Name", name).unwrap();
                    object.set(&mut cx, "Age", age).unwrap();
                    object.set(&mut cx, "AboutMe", about_me).unwrap();
                    object.set(&mut cx, "Location", location).unwrap();
                    object.set(&mut cx, "MemberSince", member_since).unwrap();
                }
                None => {}
            }
        }

        Ok(object)
    })
}
