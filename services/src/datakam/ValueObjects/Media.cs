using System;
using System.Collections.Generic;
using System.Linq;

namespace datakam.ValueObjects
{
    public class Media
    {
        public static string FILEURL = "FileUrl";
        public static string TYPE = "Type";
        public static string TAGS = "Tags";
        public static string POSITION = "Position";
        
        public string ParentDocumentID { get; set; }
        public string Type { get; set; }
        public string MediaID { get; set; }
        public string UserID { get; set; }
        public List<string> Tags { get; set; } = Enumerable.Empty<string>().ToList();
        public int Score { get; set; } = 0;
        public int Version { get; set; } = 0;
        public int Position { get; set; } = 0;
        public DateTime CreatedAt { get; set; } = new DateTime();
        public DateTime UpdatedAt { get; set; } = new DateTime();
        public string FileUrl { get; set; }
    }
}