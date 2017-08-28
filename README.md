# slapman

## GraphQL

Examples using HTTPie

* `http POST localhost:8888/graphql Content-Type:application/graphql < resources/query_hello.graphql`
* `http POST localhost:8888/graphql Content-Type:application/graphql < resources/query_stuff.graphql`
* `http POST https://9wvf5hhru0.execute-api.us-east-1.amazonaws.com/production/slapalicious query="'cat resources/query_introspection.graphql'"`
* `http POST https://9wvf5hhru0.execute-api.us-east-1.amazonaws.com/production/slapalicious query="'cat resources/query_hello_name_and_friend.graphql'"`

Native development with Nim and Rust

* `nm -D lambda/worker/worker.so | grep "T ExecuteQuery"`
* `govendor fetch -tree github.com/aws/aws-sdk-go`
* `PKG_CONFIG_ALLOW_CROSS=1 cargo build --release --target=x86_64-unknown-linux-musl`
* `nim c -r -d:release --passL:target/x86_64-unknown-linux-musl/release/libslapman.a test.nim`
* `docker run --rm -v `pwd`:/usr/src/app -w /usr/src/app nimlang/nim:alpine nim c -r -d:release --passL:-static --passL:target/x86_64-unknown-linux-musl/release/libslapman.a test.nim && strip test`
* `curl --unix-socket /tmp/some.sock http:/cmd.sh`
* `ROCKET_ENV=stage cargo run`
* `RUST_LOG=debug cargo test`

## Miscellaneous

* [Enable CORS for an API Gateway Resource](http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-cors.html)
* [How to use GraphQL with jQuery](https://www.graph.cool/docs/tutorials/graphql-and-jquery-kohj2aengo/#graphql-vs-rest)
* [Supported AWS Services](https://www.rusoto.org/supported-aws-services.html)
* [Govendor CheatSheet](https://github.com/kardianos/govendor/wiki/Govendor-CheatSheet)
* [Build Script Support](http://doc.crates.io/build-script.html)
* [Cross-compile and link a static binary on macOS for Linux with cargo and rust](https://chr4.org/blog/2017/03/15/cross-compile-and-link-a-static-binary-on-macos-for-linux-with-cargo-and-rust/)
* [Nim binary size from 160 KB to 150 Bytes](https://hookrace.net/blog/nim-binary-size/)
* [A Whirlwind Tutorial on Creating Really Teensy ELF Executables for Linux ](http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html)
* [String Functions: Nim vs Python](https://scripter.co/notes/string-functions-nim-vs-python/)
* [HTTP over Unix domain sockets in golang](https://gist.github.com/teknoraver/5ffacb8757330715bcbcc90e6d46ac74)
* [A brief look at what makes Rocket special.](https://rocket.rs/overview/)
* [Connecting a webservice to a database in Rust](http://hermanradtke.com/2016/05/23/connecting-webservice-database-rust.html)
* [Final Project: Building a Multithreaded Web Server](https://doc.rust-lang.org/book/second-edition/ch20-00-final-project-a-web-server.html)
* [Rust and Rest](http://lucumr.pocoo.org/2016/7/10/rust-rest/)
* [Conan Search for OpenSSL](https://www.conan.io/search?q=OpenSSL)
