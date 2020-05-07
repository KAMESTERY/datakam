using System.Collections;
using System.Collections.Generic;
using System.Threading.Tasks;
using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static AmazonDynamoDBClient client = new AmazonDynamoDBClient();
    }
}