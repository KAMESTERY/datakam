using System;
using System.Collections.Generic;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using static datakam.DataAccessLayer.DAL;

namespace datakam.Domain
{
    public static partial class Domain
    {
        #region RelatedMedia
        
        public static async IAsyncEnumerable<Media> GetRelatedMedia(string contentID)
        {
            await foreach (var mediaContent in GetContentByNamespace(contentID))
            {
                var media = new Media();
                media.ParentDocumentID = mediaContent.Namespace;
                media.MediaID = mediaContent.ContentID;
                media.UserID = mediaContent.UserID;
                media.Score = mediaContent.Score;
                media.Version = mediaContent.Version;
                media.CreatedAt = mediaContent.CreatedAt;
                media.UpdatedAt = mediaContent.UpdatedAt;
                
                foreach (var key in mediaContent.Keys)
                {
                    if (Media.FILEURL.Equals(key))
                        media.FileUrl = mediaContent[key];
                    if (Media.TYPE.Equals(key))
                        media.Type = Convert.ToInt32(mediaContent[key]);
                    if (Media.TAGS.Equals(key))
                        media.Tags = ((DynamoDBEntry) mediaContent[key]).AsListOfString();
                    if (Media.POSITION.Equals(key))
                        media.Position = mediaContent[key];
                }
        
                yield return media;
            }
        }
        
        #endregion
    }
}