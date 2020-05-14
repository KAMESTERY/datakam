using System.Threading.Tasks;
using datakam.ValueObjects;
using FirebaseAdmin.Auth;
using static datakam.DataAccessLayer.DAL;

namespace datakam.Domain
{
    public static partial class Domain
    {
        private static FirebaseAuth auth = FirebaseAuth.DefaultInstance;
        
        #region RegisterUser
        
        public static async Task<PutResult> RegisterUser(
            string username,
            string email,
            string password)
        {
            var userDetails = new UserRecordArgs
            {
                DisplayName = username,
                Email = email,
                Password = password
            };
            var userRecord = await auth.CreateUserAsync(userDetails);

            var existingUser = await GetUser(userRecord.Uid, userRecord.Email);
            if (existingUser == null)
            {
                var newUser = new User
                {
                    UserID = userRecord.Uid,
                    Email = email,
                    Username = userRecord.DisplayName,
                    Confirmed = userRecord.EmailVerified
                };

                var result = await PutUser(newUser);
                return new PutResult
                {
                    Count = result.Count,
                    Keys = result.Keys,
                };
            }

            return null;
        }
        
        #endregion
    }
}