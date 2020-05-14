using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using static datakam.DataAccessLayer.DAL;
using Document = datakam.ValueObjects.Document;

namespace datakam.Domain
{
    public static partial class Domain
    {
        public static async Task<PutResult> PutDocument(Document document)
        {
            var tasks = new List<Task>();
            
            var docContent = new Content
            {
                Namespace = document.Topic,
                ContentID = document.DocumentID,
                UserID = document.UserID,
                Score = document.Score,
                Version = document.Version,
                CreatedAt = document.CreatedAt,
                UpdatedAt = document.UpdatedAt,
            };
            docContent[Document.SLUG] = document.Slug;
            docContent[Document.TITLE] = document.Title;
            docContent[Document.IDENTIFIER] = document.Identifier;
            docContent[Document.BODY] = document.Body;
            docContent[Document.PUBLISH] = document.Publish;
            docContent[Document.FILTREVISUEL] = document.FiltreVisuel;
            docContent[Document.LANGUE] = document.Langue;
            docContent[Document.NIVEAU] = document.Niveau;
            docContent[Document.TAGS] = document.Tags;
            
            var putDocContentTask = PutContent(docContent);
            tasks.Add(putDocContentTask);

            var mediaContentList = document.Media.SelectMany(media =>
            {
                var mediaContentWithParent = new Content
                {
                    Namespace = document.DocumentID,
                    ContentID = media.MediaID,
                    UserID = media.UserID,
                    Score = media.Score,
                    Version = media.Version,
                    CreatedAt = media.CreatedAt,
                    UpdatedAt = media.UpdatedAt
                };
                mediaContentWithParent[Media.TAGS] = media.Tags;
                
                var mediaContentWithType = new Content
                {
                    Namespace = media.Type,
                    ContentID = media.MediaID,
                    UserID = media.UserID,
                    Score = media.Score,
                    Version = media.Version,
                    CreatedAt = media.CreatedAt,
                    UpdatedAt = media.UpdatedAt
                };
                mediaContentWithType[Media.TAGS] = media.Tags;
                
                return new List<Content>() {mediaContentWithParent, mediaContentWithType};
            });

            var batchWriteMediaContentTask = BatchWriteContent(
                mediaContentList, 
                Enumerable.Empty<ContentRef>().ToList()
            );
            tasks.Add(batchWriteMediaContentTask);

            await Task.WhenAll(tasks);
            
            return putDocContentTask.Result;
        }
        
        public static async Task<Document> GetDocument(string topic, string document_id)
        {
            var docContent = await GetContent(topic, document_id);
            
            if (docContent == null) return null;

            var document = AsDocument(docContent);
            
            await foreach(var media in GetRelatedMedia(document.DocumentID))
            {
                document.Media.Add(media);
            }

            return document;
        }
        
        public static async IAsyncEnumerable<Document> GetDocumentsByTopic(string topic)
        {
            await foreach(var docContent in GetContentByNamespace(topic))
            {
                var document = AsDocument(docContent);
                
                await foreach(var media in GetRelatedMedia(document.DocumentID))
                {
                    document.Media.Add(media);
                }
                
                yield return document;
            }
        }
        
        public static async IAsyncEnumerable<Document> GetDocumentsByDateRange(DateTime start, DateTime end)
        {
            await foreach(var docContent in GetContentByDateRange(start, end))
            {
                var document = AsDocument(docContent);
                
                await foreach(var media in GetRelatedMedia(document.DocumentID))
                {
                    document.Media.Add(media);
                }
                
                yield return document;
            }
        }
        
        public static async IAsyncEnumerable<Document> GetDocumentsByTopicAndDateRange(string ns, DateTime start, DateTime end)
        {
            await foreach(var docContent in GetContentByNamespaceAndDateRange(ns, start, end))
            {
                var document = AsDocument(docContent);
                
                await foreach(var media in GetRelatedMedia(document.DocumentID))
                {
                    document.Media.Add(media);
                }
                
                yield return document;
            }
        }
        
        public static async IAsyncEnumerable<Document> GetDocumentsByUserID(string user_id)
        {
            await foreach(var docContent in GetContentByUserID(user_id))
            {
                var document = AsDocument(docContent);
                
                await foreach(var media in GetRelatedMedia(document.DocumentID))
                {
                    document.Media.Add(media);
                }
                
                yield return document;
            }
        }

        #region AsDocument

        private static Document AsDocument(Content docContent)
        {
            var document = new Document();
            document.Topic = docContent.Namespace;
            document.DocumentID = docContent.ContentID;
            document.UserID = docContent.UserID;
            document.Score = docContent.Score;
            document.Version = docContent.Version;
            document.CreatedAt = docContent.CreatedAt;
            document.UpdatedAt = docContent.UpdatedAt;
            
            foreach (var key in docContent.Keys)
            {
                if (Document.SLUG.Equals(key))
                    document.Slug = docContent[key];
                if (Document.TITLE.Equals(key))
                    document.Title = docContent[key];
                if (Document.IDENTIFIER.Equals(key))
                    document.Identifier = docContent[key];
                if (Document.BODY.Equals(key))
                    document.Body = docContent[key];
                if (Document.PUBLISH.Equals(key))
                    document.Publish = Convert.ToBoolean(docContent[key]);
                if (Document.FILTREVISUEL.Equals(key))
                    document.FiltreVisuel = Convert.ToInt32(docContent[key]);
                if (Document.LANGUE.Equals(key))
                    document.Langue = Convert.ToInt32(docContent[key]);
                if (Document.NIVEAU.Equals(key))
                    document.Niveau = Convert.ToInt32(docContent[key]);
                if (Document.TAGS.Equals(key))
                    document.Tags = ((DynamoDBEntry) docContent[key]).AsListOfString();
            }

            return document;
        }

        #endregion
    }
}