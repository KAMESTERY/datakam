# ControlKam

## Dev Setup

```{bash}
# Install gRPC and gRPC-Web
go get -u google.golang.org/grpc
npm i grpc-web
# Install the protoc plugin for Go
go get -u github.com/golang/protobuf/protoc-gen-go
# Install the protoc compiler
brew install protobuf
# Code Generation
protoc -I ctrlkm/ ctrlkm/ctrlkm.proto --go_out=plugins=grpc:ctrlkm
# cd ctrlkm; protoc --go_out=plugins=grpc:. *.proto; cd .. # one alternative to the previous command
```

## Miscellaneous

* [gRPC -- Go Quick Start](https://grpc.io/docs/quickstart/go.html)
* [grpc-web](https://www.npmjs.com/package/grpc-web)
* [The Go language implementation of gRPC. HTTP/2 based RPC](https://github.com/grpc/grpc-go)
* [proto-lens](http://google.github.io/proto-lens/installing-protoc.html)
* [gRPC-Web is going GA](https://www.cncf.io/blog/2018/10/24/grpc-web-is-going-ga/)
* [Setting Up a New TypeScript Project](https://alligator.io/typescript/new-project/)
