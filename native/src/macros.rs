
use neon::prelude::*;

#[macro_export]
macro_rules! get_js_val {
    ( $obj:expr, $cx:expr, $field:expr, $type:ty, $default:expr ) => {
        $obj
        .get(&mut $cx, $field)?
        .downcast::<$type>()
        .unwrap_or($default)
        .value()
    };
}

#[macro_export]
macro_rules! get_js_array {
    ( $obj:expr, $cx:expr, $type:ty, $default:expr ) => {
        $obj
        .to_vec(&mut $cx)?.iter().map(|js_val| {
            let val = js_val.downcast::<$type>().unwrap_or($default).value();
            val
        })
    };
}

//use crate::dal::UserAuthData;
//use crate::validation::{
//    ADMINISTER,
//    AuthDataTrait,
//    CONTRIBUTE,
//    from_token,
//    GUEST,
//    MANAGE,
//    USER,
//    validate_token
//};

#[macro_export]
macro_rules! secured {
    ( $token:expr, $id:expr, $dowork:expr ) => {
        {
            let user_auth: Option<UserAuthData> = from_token($token);
            match user_auth {
                Some(auth) => {
                    if auth.can($id, MANAGE as i32) || auth.can($id, ADMINISTER as i32) {
                        $dowork
                    } else {
                        Ok(None)
                    }
                },
                None => {
                    debug!("Not Authorized");
                    Ok(None)
                }
            }

        }
    };
}

#[macro_export]
macro_rules! hash_map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);
