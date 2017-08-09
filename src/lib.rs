//// #[macro_use] extern crate log;
//// extern crate env_logger;
//extern crate rusoto_core;
//extern crate rusoto_s3;
// extern crate zmq;

#[no_mangle]
pub extern fn is_leap_year(year: i64) -> bool {
    let is_ly = year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
    return is_ly;
}

//#[cfg(test)]
//mod tests {
//    use rusoto_s3::{ListObjectsRequest, S3, S3Client};
//    use rusoto_core::{DefaultCredentialsProvider, Region};
//    use rusoto_core::default_tls_client;
//
//    #[test]
//    fn it_works() {
//    }
//
//    #[test]
//    fn list_objects_test() {
//        //let _ = env_logger::init(); // This initializes the `env_logger`
//
//        let bare_s3 = S3Client::new(
//            default_tls_client().unwrap(),
//            DefaultCredentialsProvider::new().unwrap(),
//            Region::UsEast1
//        );
//        let mut list_request = ListObjectsRequest::default();
//        list_request.bucket = "rusototester".to_string();
//        let result = bare_s3.list_objects(&list_request).unwrap();
//        println!("result is {:?}", result);
//
//        assert!(2*2 == 4);
//    }
//}
//
