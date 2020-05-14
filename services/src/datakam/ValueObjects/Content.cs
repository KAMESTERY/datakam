using System;
using System.Collections.Generic;

namespace datakam.ValueObjects
{
    public class Content : Dictionary<string, dynamic>
    {
        public static string NAMESPACE = "Namespace";
        public static string CONTENTID = "ContentID";
        public static string USERID = "UserID";
        public static string SCORE = "Score";
        public static string VERSION = "Version";
        public static string CREATEDAT = "CreatedAt";
        public static string UPDATEDAT = "UpdatedAt";
        
        public string Namespace { get; set; }
        public string ContentID { get; set; }
        public string UserID { get; set; }
        public int Score { get; set; } = 0;
        public int Version { get; set; } = 0;
        public DateTime CreatedAt { get; set; } = new DateTime();
        public DateTime UpdatedAt { get; set; } = new DateTime();
    }
    
    public class ContentRef : Dictionary<string, dynamic>
    {
        public string Namespace { get; set; }
        public string ContentID { get; set; }
    }
}