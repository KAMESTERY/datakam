using datakam.ValueObjects;
using GraphQL.Types;
using static datakam.Domain.Domain;

namespace datakam.MiddleWare
{
    public partial class Mutation: ObjectGraphType
    {
        public Mutation()
        {
            #region putdocstream

            FieldAsync<PutResultType>(
                "registeruser",
                arguments: new QueryArguments(
                    new QueryArgument<StringGraphType> {Name = "username", Description = "Username"},
                    new QueryArgument<StringGraphType> {Name = "email", Description = "Email"},
                    new QueryArgument<StringGraphType> {Name = "password", Description = "Password"}
                ),
                resolve: async context =>
                {
                    var username = context.GetArgument<string>("username");
                    var email = context.GetArgument<string>("email");
                    var password = context.GetArgument<string>("password");
                    if (username == null ||
                        email == null ||
                        password == null) return null;
                    var putResult = await RegisterUser(username, email, password);
                    return putResult;
                }
            );

            #endregion
            
            #region putdocument

            FieldAsync<PutResultType>(
                "putdocument",
                arguments: new QueryArguments(
                    new QueryArgument<InputDocumentType> {Name = "inputDocument", Description = "InputDocument"}
                ),
                resolve: async context =>
                {
                    var inputDocument = context.GetArgument<Document>("inputDocument");
                    if (inputDocument == null) return null;
                    var putResult = await PutDocument(inputDocument);
                    return putResult;
                }
            );

            #endregion
            
            #region putdocstream

            FieldAsync<PutResultType>(
                "putdocstream",
                arguments: new QueryArguments(
                    new QueryArgument<InputDocStreamType> {Name = "inputDocStream", Description = "InputDocStream"}
                ),
                resolve: async context =>
                {
                    var inputDocStream = context.GetArgument<DocStream>("inputDocStream");
                    if (inputDocStream == null) return null;
                    var putResult = await PutDocStream(inputDocStream);
                    return putResult;
                }
            );

            #endregion
        }
    }
}