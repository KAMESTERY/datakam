using System;

namespace datakam.ValueObjects
{
    public class UserProfile
    {
        public string UserID { get; set; }
        public string Name { get; set; }
        public int Age { get; set; }
        public string AvatarHash { get; set; }
        public string Location { get; set; }
        public DateTime MemberSince { get; set; }
        public string AboutMe { get; set; }
    }
}