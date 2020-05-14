using System.Collections.Generic;
using System.Linq;

namespace datakam.ValueObjects
{
    public class DocStream : Content
    {
        public static string TAGS = "Tags";
        
        public List<string> Tags { get; set; } = Enumerable.Empty<string>().ToList();
        // public List<dynamic> ItemsStream { get; set; } = Enumerable.Empty<dynamic>().ToList();
        
        private List<Media> _Media = Enumerable.Empty<Media>().ToList();
        public List<Media> Media { get { return _Media; } set { _Media = value; } }
        
        private List<TextBlock> _TextBlocks = Enumerable.Empty<TextBlock>().ToList();
        public List<TextBlock> TextBlocks { get { return _TextBlocks; } set { _TextBlocks = value; } }
        
        private List<dynamic> _ItemsStream = Enumerable.Empty<dynamic>().ToList();
        public List<dynamic> ItemsStream
        {
            get
            {
                if (!_ItemsStream.Any())
                {
                    _ItemsStream = Enumerable.Empty<dynamic>().ToList();
                    _ItemsStream.AddRange(Media);
                    _ItemsStream.AddRange(TextBlocks);
                }
        
                return _ItemsStream;
            }
            set
            {
                _ItemsStream = value;
            }
        }
    }
}