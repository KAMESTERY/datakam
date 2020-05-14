using System;
using System.Globalization;
using Amazon.DynamoDBv2;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static AmazonDynamoDBClient newClient() => new AmazonDynamoDBClient();
        
        private static DateTime parseDate(string dateString)
        {
            try
            {
                return DateTime.Parse(
                    dateString,
                    CultureInfo.InvariantCulture
                );
            }
            catch (FormatException e)
            {
                Console.WriteLine(e);
                return new DateTime();
            }
        }
    }
}