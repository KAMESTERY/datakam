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
