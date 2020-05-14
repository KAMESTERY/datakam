using System;
using System.Collections.Generic;
using System.Linq;

namespace datakam.ValueObjects
{
    public class TextBlock
    {
        public static string AUTHOR = "Author";
        public static string TAGS = "Tags";
        public static string TEXT = "Text";
        public static string TYPE = "Type";
        public static string POSITION = "Position";

        public string ParentDocumentID { get; set; }
        public string TextBlockID { get; set; }
        public string UserID { get; set; }
        public List<string> Tags { get; set; } = Enumerable.Empty<string>().ToList();
        public int Score { get; set; } = 0;
        public int Version { get; set; } = 0;
        public int Position { get; set; } = 0;
        public DateTime CreatedAt { get; set; } = new DateTime();
        public DateTime UpdatedAt { get; set; } = new DateTime();
        public string Type { get; set; }
        public string Text { get; set; }
        public string Author { get; set; }
    }
}