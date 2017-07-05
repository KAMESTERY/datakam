# slapman

## GraphQL

Examples using HTTPie

* `http POST localhost:8888/graphql Content-Type:application/graphql < resources/query_hello.graphql`
* `http POST localhost:8888/graphql Content-Type:application/graphql < resources/query_stuff.graphql`
* `http POST https://9wvf5hhru0.execute-api.us-east-1.amazonaws.com/production/slapalicious query="\`cat resources/query_introspection.graphql\`"`
* `http POST https://9wvf5hhru0.execute-api.us-east-1.amazonaws.com/production/slapalicious query="\`cat resources/query_hello_name_and_friend.graphql\`"`

