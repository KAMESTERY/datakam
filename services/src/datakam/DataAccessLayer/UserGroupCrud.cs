using System.Threading.Tasks;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using Document = Amazon.DynamoDBv2.DocumentModel.Document;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static Table UserGroupTable = Table.LoadTable(newClient(), "UserGroup");
        
        public static async Task<PutResult> PutUserGroup(UserGroup userGroup)
        {
            var userGroupDocument = new Document();
            userGroupDocument[UserGroup.GROUPID] = userGroup.GroupID;
            userGroupDocument[UserGroup.USERID] = userGroup.UserID;
            userGroupDocument[UserGroup.NAME] = userGroup.Name;

            var result = await UserGroupTable.PutItemAsync(userGroupDocument);
            return new PutResult
            {
                Count = result.Count,
                Keys = result.Keys,
            };
        }
        
        public static async Task<UserGroup> GetUserGroup(string group_id, string user_id)
        {
            var userGroupDocument = await UserGroupTable.GetItemAsync(group_id, user_id);
            if (userGroupDocument == null) return null;
            var userGroup = new UserGroup
            {
                GroupID = userGroupDocument[UserGroup.GROUPID],
                UserID = userGroupDocument[UserGroup.USERID],
                Name = userGroupDocument[UserGroup.NAME]
            };

            return userGroup;
        }
    }
}