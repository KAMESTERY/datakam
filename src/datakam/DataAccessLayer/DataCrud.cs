using System.Collections.Generic;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static Table DataTable = Table.LoadTable(client, "Data");
        
        public static async IAsyncEnumerable<Data> GetData(string thing_id)
        {
            var filter = new QueryFilter(Thing.THINGID, QueryOperator.Equal, thing_id);
            var search = DataTable.Query(filter);
            
            do
            {
                var documenSet = await search.GetNextSetAsync();
                foreach (var document in documenSet)
                {
                    yield return new
                        Data {
                            DataID = document["DataID"],
                            ThingID = document["ThingID"],
                            Key = document["Key"],
                            Value = document["Value"]
                        };
                }

            } while (!search.IsDone);
        }
    }
}