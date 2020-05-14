using System.Collections.Generic;
using System.Linq;

namespace datakam.ValueObjects
{
    public class PutResult
    {
        public int Count { get; set; } = 0;
        public ICollection<string> Keys { get; set; } = Enumerable.Empty<string>().ToList();
    }
}