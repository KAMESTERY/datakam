using System;

namespace datakam.ValueObjects
{
    public class User
    {
        public static string USERID = "UserID";
        public static string EMAIL = "Email";
        public static string USERNAME = "Username";
        public static string ROLE = "Role";
        public static string CONFIRMED = "Confirmed";
        public static string PASSWORDHASH = "PasswordHash";
        public static string LASTSEEN = "LastSeen";
        
        public string UserID { get; set; }
        public string Email { get; set; }
        public string Username { get; set; }
        public int Role { get; set; } = 0;
        public bool Confirmed { get; set; } = false;
        public string PasswordHash { get; set; }
        public DateTime LastSeen { get; set; } = new DateTime();
    }
}