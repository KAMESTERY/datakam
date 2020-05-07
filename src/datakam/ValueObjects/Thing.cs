using System;
using System.Collections.Generic;

namespace datakam.ValueObjects
{
    public class Thing
    {
        public static string NAME = "Name";
        public static string THINGID = "ThingID";
        
        public string Name { get; set; }
        public string ThingID { get; set; }
        public string UserID { get; set; }
        public List<string> Tags { get; set; }
        public int Score { get; set; }
        public int Version { get; set; }
        public DateTime CreatedAt { get; set; }
        public DateTime UpdatedAt { get; set; }
    }
}