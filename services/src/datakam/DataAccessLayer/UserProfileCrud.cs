using System.Threading.Tasks;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using Document = Amazon.DynamoDBv2.DocumentModel.Document;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static Table UserProfileTable = Table.LoadTable(newClient(), "UserProfile");
        
        public static async Task<PutResult> PutUserProfile(UserProfile userProfile)
        {
            var userProfileDocument = new Document();
            userProfileDocument[UserProfile.USERID] = userProfile.UserID;
            userProfileDocument[UserProfile.NAME] = userProfile.Name;
            userProfileDocument[UserProfile.AGE] = userProfile.Age;
            userProfileDocument[UserProfile.AVATARHASH] = userProfile.AvatarHash;
            userProfileDocument[UserProfile.LOCATION] = userProfile.Location;
            userProfileDocument[UserProfile.MEMBERSINCE] = userProfile.MemberSince;
            userProfileDocument[UserProfile.ABOUTME] = userProfile.AboutMe;

            var result = await UserProfileTable.PutItemAsync(userProfileDocument);
            return new PutResult
            {
                Count = result.Count,
                Keys = result.Keys,
            };
        }
        
        public static async Task<UserProfile> GetUserProfile(string user_id)
        {
            var userProfileDocument = await UserProfileTable.GetItemAsync(user_id);
            if (userProfileDocument == null) return null;
            var userProfile = new UserProfile
            {
                UserID = userProfileDocument[UserProfile.USERID],
                Name = userProfileDocument[UserProfile.NAME],
                Age = userProfileDocument[UserProfile.AGE].AsInt(),
                AvatarHash = userProfileDocument[UserProfile.AVATARHASH],
                Location = userProfileDocument[UserProfile.LOCATION],
                MemberSince = parseDate(userProfileDocument[UserProfile.MEMBERSINCE].AsString())
            };

            return userProfile;
        }
    }
}