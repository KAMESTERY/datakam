using System.Threading.Tasks;
using GraphQL;
using GraphQL.Http;
using GraphQL.Instrumentation;
using GraphQL.Types;
using GraphQL.Validation.Complexity;
using Newtonsoft.Json.Linq;

namespace datakam.MiddleWare
{
    public class GraphQLRequest
    {
        public string OperationName { get; set; }
        public string NamedQuery { get; set; }
        public string Query { get; set; }
        public JObject Variables { get; set; }
    }

    public static class Schema
    {

        private static ISchema schema = new GraphQL.Types.Schema
        {
            Query = new Query()
        };
        
        private static IDocumentExecuter executer = new DocumentExecuter();
        
        public static IDocumentWriter documentWriter = new DocumentWriter(indent: true);

        // public static async Task<string> ExecuteAsync(GraphQLRequest gqlRequest)
        public static async Task<ExecutionResult> ExecuteAsync(GraphQLRequest gqlRequest)
        {
            var result = await executer.ExecuteAsync(_ =>
            {
                _.Schema = schema;
                _.Query = gqlRequest.Query;
                _.OperationName = gqlRequest.OperationName;
                _.Inputs = gqlRequest.Variables.ToInputs();
                _.ExposeExceptions = true;
                _.ComplexityConfiguration = new ComplexityConfiguration { MaxDepth = 24 };
                _.FieldMiddleware.Use<InstrumentFieldsMiddleware>();
                _.EnableMetrics = true;
            }).ConfigureAwait(false);


            return result;

            // var json = documentWriter.Write(result);


            // var json = await schema.ExecuteAsync(_ =>
            // {
            //     _.Query = gqlRequest.Query;
            //     _.OperationName = gqlRequest.OperationName;
            //     _.Inputs = gqlRequest.Variables.ToInputs();
            //     _.ExposeExceptions = true;
            //     _.ComplexityConfiguration = new ComplexityConfiguration { MaxDepth = 24 };
            //     _.FieldMiddleware.Use<InstrumentFieldsMiddleware>();
            //     _.EnableMetrics = true;
            // });


            // return json;
        }
    }
}