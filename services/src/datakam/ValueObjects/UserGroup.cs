using System;

namespace datakam.ValueObjects
{
    public class UserGroup
    {
        public static string GROUPID = "GroupID";
        public static string USERID = "UserID";
        public static string NAME = "Name";
        
        public string GroupID { get; set; }
        public string UserID { get; set; }
        public string Name { get; set; }
    }
}