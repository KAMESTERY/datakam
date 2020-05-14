using System.Collections.Generic;
using datakam.ValueObjects;
using GraphQL.Types;
using static datakam.Domain.Domain;

namespace datakam.MiddleWare
{
    public partial class Query: ObjectGraphType
    {
        public Query()
        {
            #region getcontent

            FieldAsync<ContentType>(
                "getcontent",
                arguments: new QueryArguments(
                    new QueryArgument<StringGraphType> {Name = "topic", Description = "Topic"},
                    new QueryArgument<IdGraphType> {Name = "contentID", Description = "ContentID"}
                ),
                resolve: async context =>
                {
                    var topic = context.GetArgument<string>("topic");
                    var content_id = context.GetArgument<string>("contentID");
                    if (topic == null || content_id == null) return null;
                    
                    var document = await GetDocument(topic, content_id);
                    if (document != null) return document;
                    
                    var docStream = await GetDocStream(topic, content_id);
                    if (docStream != null)return docStream;

                    return null;
                }
            );

            #endregion
            
            #region getdocumentsbytopic

            FieldAsync<ListGraphType<ContentType>>(
                "getcontentsbytopic",
                arguments: new QueryArguments(
                    new QueryArgument<StringGraphType> {Name = "topic", Description = "Topic"}
                ),
                resolve: async context =>
                {
                    var topic = context.GetArgument<string>("topic");
                    if (topic == null) return null;
                    
                    var contentList = new List<dynamic>();
                    
                    await foreach (var document in GetDocumentsByTopic(topic))
                    {
                        contentList.Add(document);
                    }
                    
                    await foreach (var docStream in GetDocStreamsByTopic(topic))
                    {
                        contentList.Add(docStream);
                    }
                    
                    return contentList;
                }
            );

            #endregion
            
            #region getcontentsbytopics

            FieldAsync<ListGraphType<ContentType>>(
                "getcontentsbytopics",
                arguments: new QueryArguments(
                    new QueryArgument<ListGraphType<StringGraphType>> {Name = "topics", Description = "Topics"}
                ),
                resolve: async context =>
                {
                    var contentList = new List<dynamic>();
                    var topics = context.GetArgument<IEnumerable<string>>("topics");
                    if (topics == null) return contentList;
                    
                    foreach (var topic in topics)
                    {
                        await foreach (var document in GetDocumentsByTopic(topic))
                        {
                            contentList.Add(document);
                        }
                        
                        await foreach (var docStream in GetDocStreamsByTopic(topic))
                        {
                            contentList.Add(docStream);
                        }
                    }
                    
                    return contentList;
                }
            );

            #endregion
            
            // #region getdocument
            //
            // FieldAsync<DocumentType>(
            //     "getdocument",
            //     arguments: new QueryArguments(
            //         new QueryArgument<StringGraphType> {Name = "topic", Description = "Topic"},
            //         new QueryArgument<IdGraphType> {Name = "documentID", Description = "DocumentID"}
            //     ),
            //     resolve: async context =>
            //     {
            //         var topic = context.GetArgument<string>("topic");
            //         var document_id = context.GetArgument<string>("documentID");
            //         if (topic == null || document_id == null) return null;
            //         var document = await GetDocument(topic, document_id);
            //         return document;
            //     }
            // );
            //
            // #endregion
            //
            // #region getdocumentsbytopic
            //
            // FieldAsync<ListGraphType<DocumentType>>(
            //     "getdocumentsbytopic",
            //     arguments: new QueryArguments(
            //         new QueryArgument<StringGraphType> {Name = "topic", Description = "Topic"}
            //     ),
            //     resolve: async context =>
            //     {
            //         var topic = context.GetArgument<string>("topic");
            //         if (topic == null) return null;
            //         var documents = new List<Document>();
            //         await foreach (var document in GetDocumentsByTopic(topic))
            //         {
            //             documents.Add(document);
            //         }
            //         return documents;
            //     }
            // );
            //
            // #endregion
            //
            // #region getdocumentsbytopics
            //
            // FieldAsync<ListGraphType<DocumentType>>(
            //     "getdocumentsbytopics",
            //     arguments: new QueryArguments(
            //         new QueryArgument<ListGraphType<StringGraphType>> {Name = "topics", Description = "Topics"}
            //     ),
            //     resolve: async context =>
            //     {
            //         var documents = new List<Document>();
            //         var topics = context.GetArgument<IEnumerable<string>>("topics");
            //         if (topics == null) return documents;
            //         foreach (var topic in topics)
            //         {
            //             await foreach (var document in GetDocumentsByTopic(topic))
            //             {
            //                 documents.Add(document);
            //             }
            //         }
            //         
            //         return documents;
            //     }
            // );
            //
            // #endregion
            //
            // #region getdocstreamsbytopic
            //
            // FieldAsync<ListGraphType<DocStreamType>>(
            //     "getdocstreamsbytopic",
            //     arguments: new QueryArguments(
            //         new QueryArgument<StringGraphType> {Name = "topic", Description = "Topic"}
            //     ),
            //     resolve: async context =>
            //     {
            //         var topic = context.GetArgument<string>("topic");
            //         if (topic == null) return null;
            //         var docstreams = new List<DocStream>();
            //         await foreach (var docstream in GetDocStreamsByTopic(topic))
            //         {
            //             docstreams.Add(docstream);
            //         }
            //         return docstreams;
            //     }
            // );
            //
            // #endregion
        }
    }
}