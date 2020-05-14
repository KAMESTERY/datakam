using System.Collections.Generic;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using static datakam.DataAccessLayer.DAL;

namespace datakam.Domain
{
    public static partial class Domain
    {
        #region RelatedTextBlock
        
        public static async IAsyncEnumerable<TextBlock> GetRelatedTextBlock(string contentID)
        {
            await foreach (var textBlockContent in GetContentByNamespace(contentID))
            {
                var textBlock = new TextBlock();
                textBlock.ParentDocumentID = textBlockContent.Namespace;
                textBlock.TextBlockID = textBlockContent.ContentID;
                textBlock.UserID = textBlockContent.UserID;
                textBlock.Score = textBlockContent.Score;
                textBlock.Version = textBlockContent.Version;
                textBlock.CreatedAt = textBlockContent.CreatedAt;
                textBlock.UpdatedAt = textBlockContent.UpdatedAt;
                
                foreach (var key in textBlockContent.Keys)
                {
                    if (TextBlock.AUTHOR.Equals(key))
                        textBlock.Author = textBlockContent[key];
                    if (TextBlock.TAGS.Equals(key))
                        textBlock.Tags = ((DynamoDBEntry) textBlockContent[key]).AsListOfString();
                    if (TextBlock.TEXT.Equals(key))
                        textBlock.Text = textBlockContent[key];
                    if (TextBlock.TYPE.Equals(key))
                        textBlock.Type = textBlockContent[key];
                    if (TextBlock.POSITION.Equals(key))
                        textBlock.Position = textBlockContent[key];
                }
        
                yield return textBlock;
            }
        }
        
        #endregion
    }
}