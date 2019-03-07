//
// Created by outcastgeek on 3/6/2019.
//

#include <aws/core/Aws.h>
#include <aws/core/utils/Outcome.h>
#include <aws/dynamodb/DynamoDBClient.h>
#include <aws/dynamodb/model/AttributeDefinition.h>
#include <aws/dynamodb/model/CreateTableRequest.h>
#include <aws/dynamodb/model/KeySchemaElement.h>
#include <aws/dynamodb/model/ProvisionedThroughput.h>
#include <aws/dynamodb/model/ScalarAttributeType.h>
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;

    Aws::SDKOptions options;

    Aws::InitAPI(options);
    {
        const Aws::String table("NewTblFromCpp");
        const Aws::String region("us-east-1");

        Aws::Client::ClientConfiguration clientConfig;
        if (!region.empty())
            clientConfig.region = region;
        Aws::DynamoDB::DynamoDBClient dynamoClient(clientConfig);

        std::cout << "Creating table " << table <<
                  " with a composite primary key:\n" \
            "* Language - partition key\n" \
            "* Greeting - sort key\n";

        Aws::DynamoDB::Model::CreateTableRequest req;

        Aws::DynamoDB::Model::AttributeDefinition hashKey1, hashKey2;
        hashKey1.WithAttributeName("Language").WithAttributeType(Aws::DynamoDB::Model::ScalarAttributeType::S);
        req.AddAttributeDefinitions(hashKey1);
        hashKey2.WithAttributeName("Greeting").WithAttributeType(Aws::DynamoDB::Model::ScalarAttributeType::S);
        req.AddAttributeDefinitions(hashKey2);

        Aws::DynamoDB::Model::KeySchemaElement kse1, kse2;
        kse1.WithAttributeName("Language").WithKeyType(Aws::DynamoDB::Model::KeyType::HASH);
        req.AddKeySchema(kse1);
        kse2.WithAttributeName("Greeting").WithKeyType(Aws::DynamoDB::Model::KeyType::RANGE);
        req.AddKeySchema(kse2);

        Aws::DynamoDB::Model::ProvisionedThroughput thruput;
        thruput.WithReadCapacityUnits(5).WithWriteCapacityUnits(5);
        req.SetProvisionedThroughput(thruput);

        req.SetTableName(table);

        const Aws::DynamoDB::Model::CreateTableOutcome& result = dynamoClient.CreateTable(req);
        if (result.IsSuccess())
        {
            std::cout << "Table \"" << result.GetResult().GetTableDescription().GetTableName() <<
                      "\" was created!\n";
        }
        else
        {
            std::cout << "Failed to create table:" << result.GetError().GetMessage();
        }
    }
    Aws::ShutdownAPI(options);

    return 0;
}
