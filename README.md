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
* `readelf -a /path/to/some/binary | less`
* `aws dynamodb create-table --table-name myTable --attribute-definitions AttributeName=id,AttributeType=S --key-schema AttributeName=id,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5 --endpoint-url http://0.0.0.0:8000`
* `aws dynamodb list-tables --endpoint-url http://0.0.0.0:8000 --output json`
* Generate View from Template: `lein new re-frame slapman +cider +test +less +re-com +re-frisk +routes +aliases`
* Upgrade Leiningen Dependencies: `lein ancient upgrade :allow-all`

## Miscellaneous

* [Kubernetes Web UI (Dashboard)](https://kubernetes.io/docs/tasks/access-application-cluster/web-ui-dashboard/)
* [EC2 ELB PROXY protocol support](https://github.com/heptio/contour/blob/master/docs/proxy-proto.md)
* [HELM Role-based Access Control](https://github.com/helm/helm/blob/master/docs/rbac.md)
* [Configure RBAC In Your Kubernetes Cluster](https://docs.bitnami.com/kubernetes/how-to/configure-rbac-in-your-kubernetes-cluster/)
* [Kubernetes Configure Service Accounts for Pods](https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/)
* [Translate a Docker Compose File to Kubernetes Resources](https://kubernetes.io/docs/tasks/configure-pod-container/translate-compose-kubernetes/)
* [Getting Started with AWS EKS](https://www.terraform.io/docs/providers/aws/guides/eks-getting-started.html#kubernetes-worker-nodes)
* [AWS vpc](https://registry.terraform.io/modules/terraform-aws-modules/vpc/aws)
* [Announcing Terraform Support for Kubernetes Service on AWS](https://www.hashicorp.com/blog/hashicorp-announces-terraform-support-aws-kubernetes)
* [What data type should be use for timestamp in DynamoDB?](https://stackoverflow.com/questions/40561484/what-data-type-should-be-use-for-timestamp-in-dynamodb)
* [Chrono: Date and Time for Rust](https://github.com/chronotope/chrono)
* [This is a gRPC client-side load balancing example on top of grpc-go.](https://github.com/hakobe/grpc-go-client-side-load-balancing-example)
* [List of AWS Service Principals](https://gist.github.com/outcastgeek/893e45fe64b13d7a642ecfb77e03c69f)
* [AWS Policy Generator](https://awspolicygen.s3.amazonaws.com/policygen.html)
* [IAM Policy for programatic access to lightsail](https://forums.aws.amazon.com/thread.jspa?threadID=251990)
* [dive -- A tool for exploring a docker image, layer contents, and discovering ways to shrink your Docker image size.](https://github.com/wagoodman/dive)
* [How to generate an SSH key and add your public key to the server for authentication](https://adamdehaven.com/blog/how-to-generate-an-ssh-key-and-add-your-public-key-to-the-server-for-authentication/)
* [SSH-KEYGEN - GENERATE A NEW SSH KEY](https://www.ssh.com/ssh/keygen/)
* [ZODB + Traversal Wiki Tutorial](https://docs.pylonsproject.org/projects/pyramid/en/latest/tutorials/wiki/index.html)
* [Critical vulnerabilities in JSON Web Token libraries](https://auth0.com/blog/2015/03/31/critical-vulnerabilities-in-json-web-token-libraries/)
* [GO JWT Middleware](https://github.com/auth0/go-jwt-middleware/blob/master/jwtmiddleware.go)
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
* [Cobra is both a library for creating powerful modern CLI applications](go get -u github.com/spf13/cobra/cobra)
* [Amazon.Lambda.Templates](https://dotnetnew.azurewebsites.net/pack/Amazon.Lambda.Templates)
`dotnet new --install "Amazon.Lambda.Templates"`
*[AWS Lambda templates for Microsoft Template Engine accessible with the dotnet CLI's new command](https://www.nuget.org/packages/Amazon.Lambda.Templates)
`dotnet add package Amazon.Lambda.Templates --version 3.2.0`
