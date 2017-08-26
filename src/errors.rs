
use std::io;
use std::string;

use ini::ini;
use serde_json;
use url;

use api;

error_chain! {
    errors {
        QuietExit(code: i32) {
            description("slapman app quit")
        }
    }

    foreign_links {
        Io(io::Error);
        UrlParse(url::ParseError);
        Json(serde_json::Error);
        FromUtf8(string::FromUtf8Error);
        Ini(ini::Error);
        Api(api::Error);
    }
}
