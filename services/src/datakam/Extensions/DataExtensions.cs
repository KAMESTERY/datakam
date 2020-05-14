using System;

namespace datakam.Extensions
{
    public static class DateTimeExtensions
    {
        public static Guid ToGuid(this DateTime dt)
        {
            var bytes = BitConverter.GetBytes(dt.Ticks);

            Array.Resize(ref bytes, 16);

            return new Guid(bytes);
        }
    }

    public static class GuidExtensions
    {
        public static DateTime ToDateTime(this Guid guid)
        {
            var bytes = guid.ToByteArray();

            Array.Resize(ref bytes, 8);

            return new DateTime(BitConverter.ToInt64(bytes));
        }
    }
}