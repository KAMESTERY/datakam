
use dotenv;
use ini::Ini;
use log;
use std::env;
use std::fs;
use std::io;
use std::path::PathBuf;

use prelude::*;

/// Represents the auth information
#[derive(Debug, Clone)]
pub enum Auth {
    Key(String),
    Token(String),
}

pub fn prepare_environment() {
    dotenv::dotenv().ok();
}

/// Represents the `slapman-app` config.
#[derive(Debug, Clone)]
pub struct Config {
//    pub filename: PathBuf,
    pub auth: Option<Auth>,
    pub baseurl: String,
    pub log_level: log::LogLevelFilter,
    pub keep_alive: bool,
    pub verify_ssl: bool,
    pub ssl_revocation: bool,
    pub proxy_url: String,
    pub proxy_name: String,
    pub proxy_username: String,
    pub proxy_password: String,
//    pub ini: Ini,
}

impl Config {

    /// Creates a new API access config.
    pub fn new(baseurl: String) -> Config {
        Config {
            auth: Option::None,
            baseurl: baseurl,
            log_level: log::LogLevelFilter::Debug,
            keep_alive: true,
            verify_ssl: false,
            ssl_revocation: true,
            proxy_url: String::new(),
            proxy_name: String::new(),
            proxy_username: String::new(),
            proxy_password: String::new(),
        }
    }

    /// Returns the base url (without trailing slashes)
    pub fn get_base_url(&self) -> Result<&str> {
        let base = self.baseurl.trim_right_matches('/');
        if !base.starts_with("http://") && !base.starts_with("https://") {
            fail!("bad api url: unknown scheme ({})", base);
        }
        if base.matches('/').count() != 2 {
            fail!("bad api url: not on URL root ({})", base);
        }
        Ok(base)
    }

    /// Returns the API URL for a path
    pub fn get_api_endpoint(&self, path: &str) -> Result<String> {
        let base = self.get_base_url()?;
        Ok(format!("{}/api/0/{}", base, path.trim_left_matches('/')))
    }

    /// Indicates whether keepalive support should be enabled.  This
    /// mostly corresponds to an ini config but also has some sensible
    /// default handling.
    pub fn allow_keepalive(&self) -> bool {
        self.keep_alive
    }

    /// Returns the proxy URL if defined.
    fn get_proxy_url(&self) -> Option<&str> {
        Some(&self.proxy_url)
    }

    /// Returns the proxy username if defined.
    pub fn get_proxy_username(&self) -> Option<&str> {
        Some(&self.proxy_name)
    }

    /// Returns the proxy password if defined.
    pub fn get_proxy_password(&self) -> Option<&str> {
        Some(&self.proxy_password)
    }

    /// Indicates if SSL is enabled or disabled for the server.
    pub fn has_insecure_server(&self) -> bool {
        self.baseurl.starts_with("http://")
    }

    /// Indicates whether SSL verification should be on or off.
    pub fn should_verify_ssl(&self) -> bool {
        self.verify_ssl
    }

    /// Controls the SSL revocation check on windows.  This can be used as a
    /// workaround for misconfigured local SSL proxies.
    pub fn disable_ssl_revocation_check(&self) -> bool {
        self.ssl_revocation
    }
}
