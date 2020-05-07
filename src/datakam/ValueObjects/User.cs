using System;

namespace datakam.ValueObjects
{
    public class User
    {
        public string UserID { get; set; }
        public string Email { get; set; }
        public string Username { get; set; }
        public int Role { get; set; }
        public bool Confirmed { get; set; }
        public string PasswordHash { get; set; }
        public DateTime LastSeen { get; set; }
    }
}