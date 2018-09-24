use data_encoding::HEXUPPER;
use jwt::{self, Algorithm, Header, TokenData, Validation};
use ring::{digest, pbkdf2};
use serde::de::DeserializeOwned;
use serde::ser::Serialize;

const MULTI_GENIUS_KID: &'static str = "MULTI_GENIUS_KID";

const CREDENTIAL_LEN: usize = digest::SHA512_OUTPUT_LEN;
const N_ITER: u32 = 100_000;

static DIGEST_ALG: &'static digest::Algorithm = &digest::SHA512;

static PRIVATE_KEY_PEM: &'static str = include_str!("private_rsa_key.pem");
static PRIVATE_KEY_DER: &'static [u8] = include_bytes!("private_rsa_key.der");
static PUBLIC_KEY_DER: &'static [u8] = include_bytes!("public_key.der");

//    debug!("Private RSA Key Pem {:?}", PRIVATE_KEY_PEM);
//    debug!("Private RSA Key Der {:?}", PRIVATE_KEY_DER);
//    debug!("Public Key Der {:?}", PRIVATE_KEY_DER);
//    debug!("Private KeyL {:#?}", PRIVATE_KEY_DER); // Pretty Print it!!

pub fn hash_password(password: String) -> String {

    let mut pbkdf2_hash = [0u8; CREDENTIAL_LEN];

    // Create salted password
    pbkdf2::derive(DIGEST_ALG, N_ITER, PRIVATE_KEY_DER, password.as_bytes(),
                   &mut pbkdf2_hash);

    let hashed_password = HEXUPPER.encode(&pbkdf2_hash);

    hashed_password
}

pub fn check_password(hashed_password: String, password: String) -> Result<bool, &'static str> {

    // verify the hash
    let res = pbkdf2::verify(
        DIGEST_ALG,
        N_ITER,
        PRIVATE_KEY_DER,
        password.as_bytes(),
        &HEXUPPER.decode(hashed_password.as_bytes()).unwrap()
    );

    match res {
        Ok(_) => {
            debug!("Verified password!");
            Ok(true)
        },
        _ => {
            let err = "Failed to verfiy password";
            debug!("PASSWORD_CHECK_ERROR {}", err);
            Err(err)
        }
    }
}

/// T is our claims struct, and it needs to derive `Serialize` and/or `Deserialize`
pub fn jwt_encode<T>(claims: T) -> Option<String>
    where T: Serialize
{
    let mut header = Header::new(Algorithm::HS512); // TODO: Revisit this implementation to harden it using all the struct fields
    header.kid = Some(MULTI_GENIUS_KID.to_owned());
    let token = jwt::encode(&header, &claims, &PRIVATE_KEY_DER);
    token.ok()
}

pub fn jwt_decode<T: DeserializeOwned>(token: String) -> Option<T> {
    let mut validation = Validation::new(Algorithm::HS512); // TODO: Revisit this implementation to harden it using all the struct fields
    validation.validate_exp = false;
    let token_data = jwt::decode::<T>(
        &token,
        &PRIVATE_KEY_DER,
        &validation
    );
    let claims = token_data.ok()?.claims;
    Some(claims)
}
