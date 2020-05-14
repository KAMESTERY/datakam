using System.Threading.Tasks;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using Document = Amazon.DynamoDBv2.DocumentModel.Document;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static Table UserTable = Table.LoadTable(newClient(), "User");
        
        public static async Task<PutResult> PutUser(User user)
        {
            var userDocument = new Document();
            userDocument[User.USERID] = user.UserID;
            userDocument[User.EMAIL] = user.Email;
            userDocument[User.USERNAME] = user.Username;
            userDocument[User.ROLE] = user.Role;
            userDocument[User.CONFIRMED] = user.Confirmed;
            userDocument[User.PASSWORDHASH] = user.PasswordHash;
            userDocument[User.LASTSEEN] = user.LastSeen;

            var result = await UserTable.PutItemAsync(userDocument);
            return new PutResult
            {
                Count = result.Count,
                Keys = result.Keys,
            };
        }
        
        public static async Task<User> GetUser(string user_id, string email)
        {
            var userDocument = await UserTable.GetItemAsync(user_id, email);
            if (userDocument == null) return null;
            var user = new User
            {
                UserID = userDocument[User.USERID],
                Email = userDocument[User.EMAIL],
                Username = userDocument[User.USERNAME],
                Role = userDocument[User.ROLE].AsInt(),
                Confirmed = userDocument[User.CONFIRMED].AsBoolean(),
                PasswordHash = userDocument[User.PASSWORDHASH],
                LastSeen = parseDate(userDocument[User.LASTSEEN].AsString())
            };

            return user;
        }
    }
}