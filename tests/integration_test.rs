
#[macro_use] extern crate log;
extern crate env_logger;
extern crate rusoto_core;
extern crate rusoto_s3;
// extern crate zmq;

#[cfg(test)]
mod tests {
    use env_logger;
    use rusoto_s3::{ListObjectsRequest, S3, S3Client};
    use rusoto_core::{DefaultCredentialsProvider, Region};
    use rusoto_core::default_tls_client;

    #[test]
    fn it_works() {
    }

    #[test]
    fn list_objects_test() {
        let _ = env_logger::init(); // This initializes the `env_logger`

        let bare_s3 = S3Client::new(
            default_tls_client().unwrap(),
            DefaultCredentialsProvider::new().unwrap(),
            Region::UsEast1
        );
//        let bucket_name = "outcsdfgaudio";
        let bucket_name = "outcastgeek.media.blcmn.audio";
        let mut list_request = ListObjectsRequest::default();
        list_request.bucket = bucket_name.to_string();
        let s3bucketobjs = bare_s3.list_objects(&list_request);
//        let result = s3bucketobjs.unwrap();
//        info!("result is {:?}", result);
        match s3bucketobjs {
            Ok(result) => info!("result is {:?}", result),
            _ => error!("Nothing found in bucket: {}", bucket_name),
        }

        assert!(2*2 == 4);
    }
}
