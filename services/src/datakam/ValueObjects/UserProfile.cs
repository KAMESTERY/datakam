using System;

namespace datakam.ValueObjects
{
    public class UserProfile
    {
        public static string USERID = "UserID";
        public static string NAME = "Name";
        public static string AGE = "Age";
        public static string AVATARHASH = "AvatarHash";
        public static string LOCATION = "Location";
        public static string MEMBERSINCE = "MemberSince";
        public static string ABOUTME = "AboutMe";
        
        public string UserID { get; set; }
        public string Name { get; set; }
        public int Age { get; set; }
        public string AvatarHash { get; set; }
        public string Location { get; set; }
        public DateTime MemberSince { get; set; }
        public string AboutMe { get; set; }
    }
}