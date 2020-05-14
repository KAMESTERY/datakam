using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using static datakam.DataAccessLayer.DAL;

namespace datakam.Domain
{
    public static partial class Domain
    {
        #region PutDocStream
        
        public static async Task<PutResult> PutDocStream(DocStream docStream)
        {
            var tasks = new List<Task>();
            
            var docContent = new Content
            {
                Namespace = docStream.Namespace,
                ContentID = docStream.ContentID,
                UserID = docStream.UserID,
                Score = docStream.Score,
                Version = docStream.Version,
                CreatedAt = docStream.CreatedAt,
                UpdatedAt = docStream.UpdatedAt,
            };
            docContent[DocStream.TAGS] = docStream.Tags;
            
            var putDocContentTask = PutContent(docContent);
            tasks.Add(putDocContentTask);

            var docStreamContentList = docStream.ItemsStream.SelectMany<dynamic, Content>(
                (docStreamItem, index) => DocStreamItemToContent(docStream.ContentID, index, docStreamItem)
            );

            var batchWriteMediaContentTask = BatchWriteContent(
                docStreamContentList, 
                Enumerable.Empty<ContentRef>().ToList()
            );
            tasks.Add(batchWriteMediaContentTask);

            await Task.WhenAll(tasks);
            
            return putDocContentTask.Result;
        }
        
        #region PutDocStreamHelpers

        private static IEnumerable<Content> DocStreamItemToContent(string contentID, int index, Media docStreamItem)
        {
            var mediaContentWithParent = new Content
            {
                Namespace = contentID,
                ContentID = docStreamItem.MediaID,
                UserID = docStreamItem.UserID,
                Score = docStreamItem.Score,
                Version = docStreamItem.Version,
                CreatedAt = docStreamItem.CreatedAt,
                UpdatedAt = docStreamItem.UpdatedAt
            };
            mediaContentWithParent[Media.TAGS] = docStreamItem.Tags;
            mediaContentWithParent[Media.POSITION] = index;
                
            var mediaContentWithType = new Content
            {
                Namespace = docStreamItem.Type,
                ContentID = docStreamItem.MediaID,
                UserID = docStreamItem.UserID,
                Score = docStreamItem.Score,
                Version = docStreamItem.Version,
                CreatedAt = docStreamItem.CreatedAt,
                UpdatedAt = docStreamItem.UpdatedAt
            };
            mediaContentWithType[Media.TAGS] = docStreamItem.Tags;
                
            return new List<Content>() {mediaContentWithParent, mediaContentWithType};
        }
        
        private static IEnumerable<Content> DocStreamItemToContent(string contentID, int index, TextBlock docStreamItem)
        {
            var docStreamContent = new Content
            {
                Namespace = contentID,
                ContentID = docStreamItem.TextBlockID,
                UserID = docStreamItem.UserID,
                Score = docStreamItem.Score,
                Version = docStreamItem.Version,
                CreatedAt = docStreamItem.CreatedAt,
                UpdatedAt = docStreamItem.UpdatedAt
            };
            docStreamContent[TextBlock.AUTHOR] = docStreamItem.Author;
            docStreamContent[TextBlock.TAGS] = docStreamItem.Tags;
            docStreamContent[TextBlock.TEXT] = docStreamItem.Text;
            docStreamContent[TextBlock.TYPE] = docStreamItem.Type;
            docStreamContent[TextBlock.POSITION] = index;

            return new List<Content>() { docStreamContent };
        }
        
        #endregion
        
        #endregion
        
        #region GetDocStream
        
        public static async Task<DocStream> GetDocStream(string ns, string content_id)
        {
            var docContent = await GetContent(ns, content_id);

            if (docContent == null) return null;
        
            var docStream = AsDocStream(docContent);
            
            await foreach(var textBlock in GetRelatedTextBlock(docStream.ContentID))
            {
                docStream.ItemsStream.Add(textBlock);
            }
            
            await foreach(var media in GetRelatedMedia(docStream.ContentID))
            {
                docStream.ItemsStream.Add(media);
            }
        
            return docStream;
        }
        
        #endregion

        #region GetDocStreamsByTopic

        public static async IAsyncEnumerable<DocStream> GetDocStreamsByTopic(string topic)
        {
            await foreach(var docContent in GetContentByNamespace(topic))
            {
                var docStream = AsDocStream(docContent);
                
                await foreach(var textBlock in GetRelatedTextBlock(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(textBlock);
                }
                
                await foreach(var media in GetRelatedMedia(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(media);
                }
                
                yield return docStream;
            }
        }
        
        #endregion

        #region GetDocStreamsByDateRange

        public static async IAsyncEnumerable<DocStream> GetDocStreamsByDateRange(DateTime start, DateTime end)
        {
            await foreach(var docContent in GetContentByDateRange(start, end))
            {
                var docStream = AsDocStream(docContent);
                
                await foreach(var textBlock in GetRelatedTextBlock(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(textBlock);
                }
                
                await foreach(var media in GetRelatedMedia(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(media);
                }
                
                yield return docStream;
            }
        }
        
        #endregion
        
        #region GetDocStreamsByTopicAndDateRange
        
        public static async IAsyncEnumerable<DocStream> GetDocStreamsByTopicAndDateRange(string ns, DateTime start, DateTime end)
        {
            await foreach(var docContent in GetContentByNamespaceAndDateRange(ns, start, end))
            {
                var docStream = AsDocStream(docContent);
                
                await foreach(var textBlock in GetRelatedTextBlock(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(textBlock);
                }
                
                await foreach(var media in GetRelatedMedia(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(media);
                }
                
                yield return docStream;
            }
        }
        
        #endregion
        
        #region GetDocStreamsByUserID
        
        public static async IAsyncEnumerable<DocStream> GetDocStreamsByUserID(string user_id)
        {
            await foreach(var docContent in GetContentByUserID(user_id))
            {
                var docStream = AsDocStream(docContent);
                
                await foreach(var textBlock in GetRelatedTextBlock(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(textBlock);
                }
                
                await foreach(var media in GetRelatedMedia(docStream.ContentID))
                {
                    docStream.ItemsStream.Add(media);
                }
                
                yield return docStream;
            }
        }
        
        #endregion
        
        #region AsDocStream
        
        private static DocStream AsDocStream(Content docStreamContent)
        {
            var docStream = new DocStream();
            docStream.Namespace = docStreamContent.Namespace;
            docStream.ContentID = docStreamContent.ContentID;
            docStream.UserID = docStreamContent.UserID;
            docStream.Score = docStreamContent.Score;
            docStream.Version = docStreamContent.Version;
            docStream.CreatedAt = docStreamContent.CreatedAt;
            docStream.UpdatedAt = docStreamContent.UpdatedAt;
            
            foreach (var key in docStreamContent.Keys)
            {
                if (!docStream.ContainsKey(key))
                {
                    if (DocStream.TAGS.Equals(key))
                        docStream.Tags = ((DynamoDBEntry) docStreamContent[key]).AsListOfString();
                    else
                        docStream[key] = docStreamContent[key];
                }
            }
        
            return docStream;
        }
        
        #endregion
    }
}