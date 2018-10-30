
use dal::{UserAuthData};
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

#[macro_export]
macro_rules! secured {
    ( $token:expr, $id:expr, $dowork:expr ) => {
        {
            let user_auth: Option<UserAuthData> = from_token($token);
            match user_auth {
                Some(auth) => {
                    if auth.can($id, MANAGE as i32) {
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
