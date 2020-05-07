using System;
using System.Collections.Generic;

namespace datakam.ValueObjects
{
    public class Media
    {
        public static string FILEURL = "FileUrl";
        
        public string ParentDocumentID { get; set; }
        public string Type { get; set; }
        public string MediaID { get; set; }
        public string UserID { get; set; }
        public List<string> Tags { get; set; }
        public int Score { get; set; }
        public int Version { get; set; }
        public DateTime CreatedAt { get; set; }
        public DateTime UpdatedAt { get; set; }
        public string FileUrl { get; set; }
    }
}