using System;
using System.Collections.Generic;
using System.Globalization;
using System.Threading.Tasks;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static Table ThingsTable = Table.LoadTable(client, "Things");
        
        public static async Task<Thing> GetThing(string name, string thing_id)
        {
            var thingDocument = await ThingsTable.GetItemAsync(name, thing_id);
            var thing = new Thing
            {
                Name = thingDocument["Name"],
                ThingID = thingDocument["ThingID"],
                UserID = thingDocument["UserID"],
                Tags = thingDocument["Tags"].AsListOfString(),
                Score = thingDocument["Score"].AsInt(),
                Version = thingDocument["Version"].AsInt(),
                CreatedAt = parseDate(thingDocument["CreatedAt"].AsString()),
                UpdatedAt = parseDate(thingDocument["UpdatedAt"].AsString())
            };

            return thing;
        }
        
        public static async IAsyncEnumerable<Thing> GetThingByName(string name)
        {
            var filter = new QueryFilter(Thing.NAME, QueryOperator.Equal, name);
            var search = ThingsTable.Query(filter);
            
            do
            {
                var documenSet = await search.GetNextSetAsync();
                foreach (var document in documenSet)
                {
                    yield return new
                        Thing {
                            Name = document["Name"],
                            ThingID = document["ThingID"],
                            UserID = document["UserID"],
                            Tags = document["Tags"].AsListOfString(),
                            Score = document["Score"].AsInt(),
                            Version = document["Version"].AsInt(),
                            CreatedAt = parseDate(document["CreatedAt"].AsString()),
                            UpdatedAt = parseDate(document["UpdatedAt"].AsString())
                        };
                }

            } while (!search.IsDone);
        }

        private static DateTime parseDate(string dateString)
        {
            return DateTime.Parse(
                dateString,
                CultureInfo.InvariantCulture
            );
        }
    }
}