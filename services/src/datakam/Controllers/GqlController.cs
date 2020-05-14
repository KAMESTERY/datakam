using System;
using System.Text;
using System.Threading.Tasks;
using datakam.MiddleWare;
using GraphQL.Http;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using static datakam.MiddleWare.Schema;

namespace datakam.Controllers
{
    [ApiController]
    [Route("gql")]
    [Consumes("application/json")]
    [Produces("application/json")]
    public class GqlController: ControllerBase
    {
        private readonly ILogger _logger;

        public GqlController(ILogger<GqlController> logger)
        {
            _logger = logger;
        }
        
        public async Task<IActionResult> PostAsync([FromBody] GraphQLRequest gqlRequest)
        {
            var result = await ExecuteAsync(gqlRequest);
            if (result.Errors?.Count > 0)
            {
                var json = await documentWriter.WriteToStringAsync(
                    result,
                    Encoding.UTF8
                );
                _logger.LogError(json);
                return StatusCode(
                    StatusCodes.Status400BadRequest,
                    result.Errors
                );
            }

            return Ok(new
            {
                Data = result.Data
            });
            
            // var json = await documentWriter.WriteToStringAsync(
            //     result,
            //     Encoding.UTF8
            // );
            // return Ok(json);
        }
        
    }
}