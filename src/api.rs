
use chrono::prelude::Utc;
use chrono::{Duration, DateTime};
use config::{Config, Auth};
use constants::{VERSION};
use curl;
use indicatif::ProgressBar;
//use openssl_probe;
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json;
use std::ascii::AsciiExt;
use std::borrow::Cow;
use std::cell::{RefMut, RefCell};
use std::collections::{HashSet, HashMap};
use std::error;
use std::fmt;
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::path::Path;
use std::rc::Rc;
use std::str;
use std::thread;
use regex::{Regex, Captures};
use url::percent_encoding::{utf8_percent_encode, DEFAULT_ENCODE_SET};
use utils;

/// Wrapper that escapes arguments for URL path segments
pub struct PathArg<A: fmt::Display>(A);

impl<A: fmt::Display> fmt::Display for PathArg<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // if we put values into the path we need to url encode them. However
        // special care needs to be taken for any slash character or path
        // segments that would end up as ".." or "." for security reasons.
        // Since we cannot handle slashed there we just replace them with the
        // unicode replacement character as a quick workaround. This will
        // typically result in 404s from the server.
        let mut val = format!("{}", self.0).replace('/', "\u{fffd}");
        if val == ".." || val == "." {
            val = "\u{fffd}".into();
        }
        utf8_percent_encode(&val, DEFAULT_ENCODE_SET).fmt(f)
    }
}

#[derive(PartialEq, Eq)]
pub enum ProgressBarMode {
    Disabled,
    Request,
    Response,
    Both,
}

/// Helper for the API access.
pub struct Api<'a> {
    config: &'a Config,
    shared_handle: RefCell<curl::easy::Easy>,
}

/// Represents file contents temporarily
pub enum FileContents<'a> {
    FromPath(&'a Path),
    FromBytes(&'a [u8]),
}

/// Represents API errors.
#[derive(Debug)]
pub enum Error {
    Http(u32, String),
    Curl(curl::Error),
    Form(curl::FormError),
    Io(io::Error),
    Json(serde_json::Error),
    ResourceNotFound(&'static str),
    BadApiUrl(String),
}

/// Shortcut alias for results of this module.
pub type ApiResult<T> = Result<T, Error>;

/// Represents an HTTP method that is used by the API.
#[derive(PartialEq, Debug)]
pub enum Method {
    Get,
    Head,
    Post,
    Put,
    Delete,
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Method::Get => write!(f, "GET"),
            Method::Head => write!(f, "HEAD"),
            Method::Post => write!(f, "POST"),
            Method::Put => write!(f, "PUT"),
            Method::Delete => write!(f, "DELETE"),
        }
    }
}

/// Represents an API request. This can be customized before
/// sending but only sent once.
pub struct ApiRequest<'a> {
    handle: RefMut<'a, curl::easy::Easy>,
    headers: curl::easy::List,
    body: Option<Vec<u8>>,
    progress_bar_mode: ProgressBarMode,
}

/// Represents and API response.
#[derive(Clone, Serialize, Debug)]
pub struct ApiResponse {
    status: u32,
    headers: Vec<String>,
    body: Option<Vec<u8>>,
}

impl<'a> Api<'a> {
    /// Creates a new API access helper for the given config. For as long
    /// as it lives HTTP keepalive can be used. When the object is recreated
    /// new connections will be established.
    pub fn new(config: &'a Config) -> Api<'a> {
        Api {
            config: config,
            shared_handle: RefCell::new(curl::easy::Easy::new()),
        }
    }

    // Low Level Methods

    /// Create a new `ApiRequest` for the given HTTP method and URL.  If the
    /// URL is just a path then it's relative to the configured API host
    /// and authentication is automatically enabled.
    pub fn request(&'a self, method: Method, url: &str) -> ApiResult<ApiRequest<'a>> {
        let mut handle = self.shared_handle.borrow_mut();
        if !self.config.should_verify_ssl() {
            handle.forbid_reuse(true).ok();
        }
        handle.reset();
        let mut ssl_opts = curl::easy::SslOpt::new();
        if self.config.disable_ssl_revocation_check() {
            ssl_opts.no_revoke(true);
        }
        handle.ssl_options(&ssl_opts)?;
        let (url, auth) = if url.starts_with("http://") || url.starts_with("https://") {
            (Cow::Borrowed(url), None)
        } else {
            (Cow::Owned(match self.config.get_api_endpoint(url) {
                Ok(rv) => rv,
                Err(err) => {
                    return Err(Error::BadApiUrl(err.to_string()));
                }
            }), self.config.auth.as_ref())
        };

        // the proxy url is discovered from the http_proxy envvar.
        if let Some(proxy_username) = self.config.get_proxy_username() {
            handle.proxy_username(proxy_username)?;
        }
        if let Some(proxy_password) = self.config.get_proxy_password() {
            handle.proxy_password(proxy_password)?;
        }
        handle.ssl_verify_host(self.config.should_verify_ssl())?;

        ApiRequest::new(handle, method, &url, auth)
    }

    /// Convenience method that performs a `GET` request.
    pub fn get(&self, path: &str) -> ApiResult<ApiResponse> {
        self.request(Method::Get, path)?.send()
    }

    /// Convenience method that performs a `DELETE` request.
    pub fn delete(&self, path: &str) -> ApiResult<ApiResponse> {
        self.request(Method::Delete, path)?.send()
    }

    /// Convenience method that performs a `POST` request with JSON data.
    pub fn post<S: Serialize>(&self, path: &str, body: &S) -> ApiResult<ApiResponse> {
        self.request(Method::Post, path)?.with_json_body(body)?.send()
    }

    /// Convenience method that performs a `PUT` request with JSON data.
    pub fn put<S: Serialize>(&self, path: &str, body: &S) -> ApiResult<ApiResponse> {
        self.request(Method::Put, path)?.with_json_body(body)?.send()
    }

    /// Convenience method that downloads a file into the given file object.
    pub fn download(&self, url: &str, dst: &mut fs::File) -> ApiResult<ApiResponse> {
        self.request(Method::Get, &url)?.follow_location(true)?.send_into(dst)
    }

    /// Convenience method that downloads a file into the given file object
    /// and show a progress bar
    pub fn download_with_progress(&self, url: &str, dst: &mut fs::File) -> ApiResult<ApiResponse> {
        self.request(Method::Get, &url)?
            .follow_location(true)?
            .progress_bar_mode(ProgressBarMode::Response)?
            .send_into(dst)
    }

    /// Convenience method that waits for a few seconds until a resource
    /// becomes available.
    pub fn wait_until_available(&self, url: &str, duration: Duration) -> ApiResult<bool> {
        let started = Utc::now();
        loop {
            match self.request(Method::Get, &url)?.send() {
                Ok(_) => { return Ok(true); }
                Err(err) => {
                    match err {
                        Error::Http(..) | Error::Curl(..) => {}
                        err => { return Err(err); }
                    }
                }
            }
            thread::sleep(Duration::milliseconds(500).to_std().unwrap());
            if Utc::now() - duration > started {
                return Ok(false);
            }
        }
    }

    // High Level Methods

    /// Performs an API request to verify the authentication status of the
    /// current token.
    pub fn get_auth_info(&self) -> ApiResult<AuthInfo> {
        self.get("/")?.convert()
    }
}

fn send_req<W: Write>(handle: &mut curl::easy::Easy,
                      out: &mut W,
                      body: Option<Vec<u8>>,
                      progress_bar_mode: ProgressBarMode)
                      -> ApiResult<(u32, Vec<String>)> {
    match body {
        Some(body) => {
            let mut body = &body[..];
            handle.upload(true)?;
            handle.in_filesize(body.len() as u64)?;
            handle_req(handle, out, progress_bar_mode,
                       &mut |buf| body.read(buf).unwrap_or(0))
        }
        None => handle_req(handle, out, progress_bar_mode, &mut |_| 0),
    }
}

fn handle_req<W: Write>(handle: &mut curl::easy::Easy,
                        out: &mut W,
                        progress_bar_mode: ProgressBarMode,
                        read: &mut FnMut(&mut [u8]) -> usize)
                        -> ApiResult<(u32, Vec<String>)> {
    if progress_bar_mode != ProgressBarMode::Disabled {
        handle.progress(true)?;
    }

    // enable verbose mode
    handle.verbose(true)?;

    let mut headers = Vec::new();
    let pb : Rc<RefCell<Option<ProgressBar>>> = Rc::new(RefCell::new(None));
    {
        let mut headers = &mut headers;
        let mut handle = handle.transfer();

        if progress_bar_mode != ProgressBarMode::Disabled {
            let pb_progress = pb.clone();
            handle.progress_function(move |a, b, c, d| {
                let (down_len, down_pos, up_len, up_pos) =
                    (a as u64, b as u64, c as u64, d as u64);
                let mut pb = pb_progress.borrow_mut();
                if up_len > 0 && (progress_bar_mode == ProgressBarMode::Request ||
                                  progress_bar_mode == ProgressBarMode::Both) {
                    if up_pos < up_len {
                        if pb.is_none() {
                            *pb = Some(utils::make_byte_progress_bar(up_len));
                        }
                        pb.as_ref().unwrap().set_position(up_pos);
                    } else if pb.is_some() {
                        pb.take().unwrap().finish_and_clear();
                    }
                }
                if down_len > 0 && (progress_bar_mode == ProgressBarMode::Response ||
                                    progress_bar_mode == ProgressBarMode::Both) {
                    if down_pos < down_len {
                        if pb.is_none() {
                            *pb = Some(utils::make_byte_progress_bar(down_len));
                        }
                        pb.as_ref().unwrap().set_position(down_pos);
                    } else if pb.is_some() {
                        pb.take().unwrap().finish_and_clear();
                    }
                }
                true
            })?;
        }

        handle.read_function(move |buf| Ok(read(buf)))?;

        handle.write_function(move |data| {
            Ok(match out.write_all(data) {
                Ok(_) => data.len(),
                Err(_) => 0,
            })
        })?;

        handle.debug_function(move |info, data| {
            match info {
                curl::easy::InfoType::HeaderIn => {
                    log_headers(false, data);
                },
                curl::easy::InfoType::HeaderOut => {
                    log_headers(true, data);
                }
                _ => {}
            }
        })?;

        handle.header_function(move |data| {
            headers.push(String::from_utf8_lossy(data).into_owned());
            true
        })?;
        handle.perform()?;
    }

    if pb.borrow().is_some() {
        pb.borrow().as_ref().unwrap().finish_and_clear();
    }

    Ok((handle.response_code()?, headers))
}

/// Iterator over response headers
#[allow(dead_code)]
pub struct Headers<'a> {
    lines: &'a [String],
    idx: usize,
}

impl<'a> Iterator for Headers<'a> {
    type Item = (&'a str, &'a str);

    fn next(&mut self) -> Option<(&'a str, &'a str)> {
        self.lines.get(self.idx).map(|line| {
            self.idx += 1;
            match line.find(':') {
                Some(i) => (&line[..i], line[i + 1..].trim()),
                None => (line[..].trim(), ""),
            }
        })
    }
}

impl<'a> ApiRequest<'a> {
    fn new(mut handle: RefMut<'a, curl::easy::Easy>,
           method: Method,
           url: &str,
           auth: Option<&Auth>)
           -> ApiResult<ApiRequest<'a>> {
        info!("request {} {}", method, url);

        let mut headers = curl::easy::List::new();
        headers.append("Expect:").ok();
        headers.append(&format!("User-Agent: sentry-cli/{}", VERSION)).ok();

        match method {
            Method::Get => handle.get(true)?,
            Method::Head => {
                handle.get(true)?;
                handle.custom_request("HEAD")?;
                handle.nobody(true)?;
            },
            Method::Post => handle.custom_request("POST")?,
            Method::Put => handle.custom_request("PUT")?,
            Method::Delete => handle.custom_request("DELETE")?,
        }

        handle.url(&url)?;
        match auth {
            None => {}
            Some(&Auth::Key(ref key)) => {
                handle.username(key)?;
                info!("using key based authentication");
            }
            Some(&Auth::Token(ref token)) => {
                headers.append(&format!("Authorization: Bearer {}", token))?;
                info!("using token authentication");
            }
        }

        Ok(ApiRequest {
            handle: handle,
            headers: headers,
            body: None,
            progress_bar_mode: ProgressBarMode::Disabled,
        })
    }

    /// adds a specific header to the request
    pub fn with_header(mut self, key: &str, value: &str) -> ApiResult<ApiRequest<'a>> {
        self.headers.append(&format!("{}: {}", key, value))?;
        Ok(self)
    }

    /// sets the JSON request body for the request.
    pub fn with_json_body<S: Serialize>(mut self, body: &S) -> ApiResult<ApiRequest<'a>> {
        let mut body_bytes: Vec<u8> = vec![];
        serde_json::to_writer(&mut body_bytes, &body)?;
        info!("sending JSON data ({} bytes)", body_bytes.len());
        self.body = Some(body_bytes);
        self.headers.append("Content-Type: application/json")?;
        Ok(self)
    }

    /// attaches some form data to the request.
    pub fn with_form_data(mut self, form: curl::easy::Form) -> ApiResult<ApiRequest<'a>> {
        info!("sending form data");
        self.handle.httppost(form)?;
        self.body = None;
        Ok(self)
    }

    /// enables or disables redirects.  The default is off.
    pub fn follow_location(mut self, val: bool) -> ApiResult<ApiRequest<'a>> {
        info!("follow redirects: {}", val);
        self.handle.follow_location(val)?;
        Ok(self)
    }

    /// enables a progress bar.
    pub fn progress_bar_mode(mut self, mode: ProgressBarMode) -> ApiResult<ApiRequest<'a>> {
        self.progress_bar_mode = mode;
        Ok(self)
    }

    /// Sends the request and writes response data into the given file
    /// instead of the response object's in memory buffer.
    pub fn send_into<W: Write>(mut self, out: &mut W) -> ApiResult<ApiResponse> {
        self.handle.http_headers(self.headers)?;
        let (status, headers) = send_req(
            &mut self.handle, out, self.body, self.progress_bar_mode)?;
        info!("response: {}", status);
        Ok(ApiResponse {
            status: status,
            headers: headers,
            body: None,
        })
    }

    /// Sends the request and reads the response body into the response object.
    pub fn send(self) -> ApiResult<ApiResponse> {
        let mut out = vec![];
        let mut rv = self.send_into(&mut out)?;
        rv.body = Some(out);
        Ok(rv)
    }
}

impl ApiResponse {
    /// Returns the status code of the response
    pub fn status(&self) -> u32 {
        self.status
    }

    /// Indicates that the request failed
    pub fn failed(&self) -> bool {
        self.status >= 400 && self.status <= 600
    }

    /// Indicates that the request succeeded
    pub fn ok(&self) -> bool {
        !self.failed()
    }

    /// Converts the API response into a result object.  This also converts
    /// non okay response codes into errors.
    pub fn to_result(self) -> ApiResult<ApiResponse> {
        if let Some(ref body) = self.body {
            info!("body: {}", String::from_utf8_lossy(body));
        }
        if self.ok() {
            return Ok(self);
        }
        if let Ok(err) = self.deserialize::<ErrorInfo>() {
            if let Some(detail) = err.detail.or(err.error) {
                fail!(Error::Http(self.status(), detail));
            }
        }
        fail!(Error::Http(self.status(), "generic error".into()));
    }

    /// Deserializes the response body into the given type
    pub fn deserialize<T: DeserializeOwned>(&self) -> ApiResult<T> {
        Ok(serde_json::from_reader(match self.body {
            Some(ref body) => body,
            None => &b""[..],
        })?)
    }

    /// Like `deserialize` but consumes the response and will convert
    /// failed requests into proper errors.
    pub fn convert<T: DeserializeOwned>(self) -> ApiResult<T> {
        self.to_result().and_then(|x| x.deserialize())
    }

    /// Like convert but produces resource not found errors.
    pub fn convert_rnf<T: DeserializeOwned>(self, resource: &'static str) -> ApiResult<T> {
        if self.status() == 404 {
            return Err(Error::ResourceNotFound(resource))
        } else {
            self.to_result().and_then(|x| x.deserialize())
        }
    }

    /// Iterates over the headers.
    #[allow(dead_code)]
    pub fn headers(&self) -> Headers {
        Headers {
            lines: &self.headers[..],
            idx: 0,
        }
    }

    /// Looks up the first matching header for a key.
    #[allow(dead_code)]
    pub fn get_header(&self, key: &str) -> Option<&str> {
        for (header_key, header_value) in self.headers() {
            if header_key.eq_ignore_ascii_case(key) {
                return Some(header_value);
            }
        }
        None
    }
}

fn log_headers(is_response: bool, data: &[u8]) {
    lazy_static! {
        static ref AUTH_RE: Regex = Regex::new(
            r"(?i)(authorization):\s*([\w]+)\s+(.*)").unwrap();
    }
    if let Ok(header) = str::from_utf8(data) {
        for line in header.lines() {
            if line.is_empty() {
                continue;
            }
            info!("{} {}", if is_response {
                ">"
            } else {
                "<"
            }, AUTH_RE.replace_all(line, |caps: &Captures| {
                let info = if &caps[1].to_lowercase() == "basic" {
                    caps[3].split(':').next().unwrap().to_string()
                } else {
                    format!("{}***", &caps[3][..8])
                };
                format!("{}: {} {}", &caps[1], &caps[2], info)
            }));
        }
    }
}


impl error::Error for Error {
    fn description(&self) -> &str {
        "api error"
    }
}

impl From<curl::FormError> for Error {
    fn from(err: curl::FormError) -> Error {
        Error::Form(err)
    }
}

impl From<curl::Error> for Error {
    fn from(err: curl::Error) -> Error {
        Error::Curl(err)
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Error {
        Error::Json(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Http(status, ref msg) => write!(f, "http error: {} ({})", msg, status),
            Error::Curl(ref err) => write!(f, "http error: {}", err),
            Error::Form(ref err) => write!(f, "http form error: {}", err),
            Error::Io(ref err) => write!(f, "io error: {}", err),
            Error::Json(ref err) => write!(f, "bad json: {}", err),
            Error::ResourceNotFound(res) => write!(f, "{} not found", res),
            Error::BadApiUrl(ref msg) => write!(f, "{}", msg),
        }
    }
}

#[derive(Debug, Deserialize)]
struct ErrorInfo {
    detail: Option<String>,
    error: Option<String>,
}

/// Provides the auth details (access scopes)
#[derive(Deserialize, Debug)]
pub struct AuthDetails {
    pub scopes: Vec<String>,
}

/// Indicates which user signed in
#[derive(Deserialize, Debug)]
pub struct User {
    pub email: String,
    pub id: String,
}

/// Provides the authentication information
#[derive(Deserialize, Debug)]
pub struct AuthInfo {
    pub auth: Option<AuthDetails>,
    pub user: Option<User>,
}
