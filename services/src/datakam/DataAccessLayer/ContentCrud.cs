using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Amazon.DynamoDBv2.DocumentModel;
using datakam.ValueObjects;
using Document = Amazon.DynamoDBv2.DocumentModel.Document;

namespace datakam.DataAccessLayer
{
    public static partial class DAL
    {
        private static Table ContentTable = Table.LoadTable(newClient(), "Content");

        public static async Task<PutResult> PutContent(Content content)
        {
            var contentDocument = ContentToDocument(content);

            var result = await ContentTable.PutItemAsync(contentDocument);
            return new PutResult
            {
                Count = result.Count,
                Keys = result.Keys,
            };
        }

        public static async Task BatchWriteContent(
            IEnumerable<Content> contentPutList,
            IEnumerable<ContentRef> contentDeleteList)
        {
            var documentPutList = contentPutList.Select(
                content => ContentToDocument(content)
            );
            var documentBatchWrite = ContentTable.CreateBatchWrite();
            foreach (var document in documentPutList)
            {
                documentBatchWrite.AddDocumentToPut(document);
            }
            
            var documentDeleteList = contentDeleteList.Select(contentRef => {
                var contentRefDocument = new Document();
                contentRefDocument[Content.NAMESPACE] = contentRef.Namespace;
                contentRefDocument[Content.CONTENTID] = contentRef.ContentID;
                return contentRefDocument;
            });
            foreach (var document in documentDeleteList)
            {
                documentBatchWrite.AddItemToDelete(document);
            }

            await documentBatchWrite.ExecuteAsync();
        }
        
        private static Document ContentToDocument(Content content)
        {
            var contentDocument = new Document();
            contentDocument[Content.NAMESPACE] = content.Namespace;
            contentDocument[Content.CONTENTID] = content.ContentID;
            contentDocument[Content.USERID] = content.UserID;
            contentDocument[Content.SCORE] = content.Score;
            contentDocument[Content.VERSION] = content.Version;
            contentDocument[Content.CREATEDAT] = content.CreatedAt;
            contentDocument[Content.UPDATEDAT] = content.UpdatedAt;
            foreach (var key in content.Keys)
            {
                contentDocument[key] = content[key];
            }

            return contentDocument;
        }
        
        public static async Task<Content> GetContent(string ns, string content_id)
        {
            var contentDocument = await ContentTable.GetItemAsync(ns, content_id);
            if (contentDocument == null) return null;
            var content = new Content
            {
                Namespace = contentDocument["Namespace"],
                ContentID = contentDocument["ContentID"],
                UserID = contentDocument["UserID"],
                Score = contentDocument["Score"].AsInt(),
                Version = contentDocument["Version"].AsInt(),
                CreatedAt = parseDate(contentDocument["CreatedAt"].AsString()),
                UpdatedAt = parseDate(contentDocument["UpdatedAt"].AsString())
            };

            foreach (var key in contentDocument.Keys)
            {
                if (!content.ContainsKey(key))
                    content[key] = contentDocument[key];
            }

            return content;
        }
        
        public static async IAsyncEnumerable<Content> GetContentByNamespace(string name)
        {
            var filter = new QueryFilter(Content.NAMESPACE, QueryOperator.Equal, name);
            await foreach (var content in RunQuery(filter))
            {
                yield return content;
            }
        }
        
        public static async IAsyncEnumerable<Content> GetContentByNamespaceAndDateRange(string ns, DateTime start, DateTime end)
        {
            var filter = new QueryFilter(Content.NAMESPACE, QueryOperator.Equal, ns);
            filter.AddCondition(Content.UPDATEDAT, QueryOperator.Between, start, end);
            await foreach (var content in RunQuery(filter))
            {
                yield return content;
            }
        }
        
        public static async IAsyncEnumerable<Content> GetContentByUserID(string user_id)
        {
            var filter = new QueryFilter(Content.USERID, QueryOperator.Equal, user_id);
            await foreach (var content in RunQuery(filter))
            {
                yield return content;
            }
        }
        
        public static async IAsyncEnumerable<Content> GetContentByUserIDAndDateRange(string user_id, DateTime start, DateTime end)
        {
            var filter = new QueryFilter(Content.USERID, QueryOperator.Equal, user_id);
            filter.AddCondition(Content.UPDATEDAT, QueryOperator.Between, start, end);
            await foreach (var content in RunQuery(filter))
            {
                yield return content;
            }
        }
        
        public static async IAsyncEnumerable<Content> GetContentByDateRange(DateTime start, DateTime end)
        {
            var filter = new QueryFilter(Content.UPDATEDAT, QueryOperator.Between, start, end);
            await foreach (var content in RunQuery(filter))
            {
                yield return content;
            }
        }

        private static async IAsyncEnumerable<Content> RunQuery(QueryFilter filter)
        {
            var search = ContentTable.Query(filter);
            
            do
            {
                var documenSet = await search.GetNextSetAsync();
                foreach (var document in documenSet)
                {
                    var content = new Content {
                        Namespace = document[Content.NAMESPACE],
                        ContentID = document[Content.CONTENTID],
                        UserID = document[Content.USERID],
                        Score = document[Content.SCORE].AsInt(),
                        Version = document[Content.VERSION].AsInt(),
                        CreatedAt = parseDate(document[Content.CREATEDAT].AsString()),
                        UpdatedAt = parseDate(document[Content.UPDATEDAT].AsString())
                    };
                    foreach (var key in document.Keys)
                    {
                        if (!content.ContainsKey(key))
                            content[key] = document[key];
                    }
                    yield return content;
                }

            } while (!search.IsDone);
        }
    }
}