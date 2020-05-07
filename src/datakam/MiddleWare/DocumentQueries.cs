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
            #region getdocument

            FieldAsync<DocumentType>(
                "getdocument",
                arguments: new QueryArguments(
                    new QueryArgument<IdGraphType> {Name = "topic", Description = "Topic"},
                    new QueryArgument<IdGraphType> {Name = "documentID", Description = "DocumentID"}
                ),
                resolve: async context =>
                {
                    var topic = context.GetArgument<string>("topic");
                    var document_id = context.GetArgument<string>("documentID");
                    var document = await GetDocument(topic, document_id);
                    return document;
                }
            );

            #endregion

            #region getdocumentsbytopic

            FieldAsync<ListGraphType<DocumentType>>(
                "getdocumentsbytopic",
                arguments: new QueryArguments(
                    new QueryArgument<IdGraphType> {Name = "topic", Description = "Topic"}
                ),
                resolve: async context =>
                {
                    var topic = context.GetArgument<string>("topic");
                    var documents = new List<Document>();
                    await foreach (var document in GetDocumentsByTopic(topic))
                    {
                        documents.Add(document);
                    }
                    return documents;
                }
            );

            #endregion
        }
    }
}